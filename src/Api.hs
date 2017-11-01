{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Api
    (
    api
    ) where

import           Auth
import           Lib
import qualified Model
import           ModelExt                             (buildExt)
import           ModelExt                             as Model
import           Utils

import           AuthMiddleware
import           Control.Monad.Reader                 (ReaderT (..), asks,
                                                       liftIO, unless, when)
import           Control.Monad.Trans.Class            (lift)
import qualified Data.ByteString.Lazy.Char8           as S8
import           Data.Maybe
import qualified Data.Text.Lazy                       as T
import           Data.Time.Clock.POSIX
import           Database.Persist.Postgresql          ((<-.), (=.), (==.))
import qualified Database.Persist.Postgresql          as DB
import           Network.Wai                          (Middleware,
                                                       mapResponseHeaders,
                                                       modifyResponse, pathInfo)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty                           (capture)
import qualified Web.Scotty.Trans                     as S

default(T.Text)

secret = S8.pack "secret"
expired = 60 * 5

protectedResources :: AuthFilter
protectedResources request = return $ protect path where
    path = pathInfo request
    protect ("api" : "v1" : "realm" : _) =  True
    protect _                            =  False

withSomeHeader :: Middleware
withSomeHeader =
    modifyResponse (mapResponseHeaders (("Content-Language", "en-US") :))

mkapi :: (DoReturn a, Monad m, S.ScottyError a) => (S.RoutePattern -> S.ActionT a m () -> c) -> String -> S.ActionT a m () -> c
mkapi method _name = (method . capture $ "/api/v1/" ++ _name) . (`S.rescue` doFinish)
mkrealm :: (DoReturn a, Monad m, S.ScottyError a) => (S.RoutePattern -> S.ActionT a m () -> c) -> String -> S.ActionT a m () -> c
mkrealm method _name = (method . capture $ "/api/v1/realm/" ++ _name) . (`S.rescue` doFinish)

currentUser :: S.ScottyError e => S.ActionT e ConfigM (Maybe Auth.Payload)
currentUser = do
    req <- S.request
    key <- lift (asks userKey)
    return $ getUser key req

justCurrentUser :: S.ScottyError e => S.ActionT e ConfigM Auth.Payload
justCurrentUser = do
    vuser   <- currentUser
    unless (isJust vuser) $ doFinish "Permission Denied"
    return $ fromJust vuser

justAdminUser :: S.ScottyError e => S.ActionT e ConfigM Auth.Payload
justAdminUser = do
    user    <- justCurrentUser
    unless (Auth.checkAdmin user) $ doFinish "Permission Denied"
    return user

class DB.PersistEntity target => FetchByName target where
    uniqueMethod :: String -> DB.Unique target
    fetchByName :: (DB.PersistEntityBackend target ~ DB.BaseBackend backend, DB.PersistUniqueRead backend) => String -> ReaderT backend IO (DB.Entity target)
    fetchByName _name = fromJust <$> DB.getBy (uniqueMethod _name)

instance FetchByName Model.Node where
    uniqueMethod = Model.UniqueNodeName

instance FetchByName Model.User where
    uniqueMethod = Model.UniqueUsername

instance FetchByName Model.Tag where
    uniqueMethod = Model.UniqueTagName

loadString :: (Monad m, S.ScottyError e) => T.Text -> S.ActionT e m String
loadString x = T.unpack . T.toLower . T.strip <$> S.param x

api :: Config -> S.ScottyT T.Text ConfigM ()
api cfg = let
    authSetting = "test" { authIsProtected = protectedResources }
    in do
    S.middleware withSomeHeader
    S.middleware logStdoutDev
    S.middleware (gzip def)
    S.middleware (tokenAuth (userKey cfg) authSetting)

    mkapi S.post "register" $ do
        username    <- T.unpack . T.toLower . T.strip <$> S.param "username"
        password    <- S.param "password"
        time        <- liftIO getCurrentTime
        result      <- Lib.runDB (DB.insertUnique $ Model.User username password False time time)
        doReturn $ maybe (Left "Register Failed") Right result

    mkapi S.post "login" $ let
        proc username (Just entity) = Right . S8.unpack . Auth.generateToken secret <$> Auth.makePayload (DB.fromSqlKey $ DB.entityKey entity, username, Model.userAdmin $ DB.entityVal entity) expired
        proc _ Nothing = return $ Left "Login Failed"
        check (username, password) = Lib.runDB (DB.selectFirst [Model.UserUsername ==. username, Model.UserPassword ==. password] [])
        in do
        username    <- loadString "username"
        password    <- S.param "password"
        doReturn =<< S.liftAndCatchIO . proc username =<< check (username, password)

    mkrealm S.get "whoami" $ doReturn =<< justCurrentUser

    mkrealm S.put "user" $ do
        user    <- justCurrentUser
        oldpass <- S.param "oldpass"
        newpass <- S.param "newpass"
        doReturn =<< Lib.runDB (Count <$> DB.updateWhereCount [Model.UserId ==. DB.toSqlKey (Auth.userId user), Model.UserPassword ==. oldpass] [Model.UserPassword =. newpass])

    mkrealm S.get "node" $ doReturn =<< Lib.runDB (DB.selectList [Model.NodeParent ==. Nothing] [] :: DB.SqlPersistT IO [DB.Entity Model.Node])

    mkrealm S.post "node" $ do
        _       <- justAdminUser
        title   <- loadString "title"
        desc    <- loadString "description"
        time    <- liftIO getCurrentTime
        result  <- Lib.runDB (DB.insertUnique $ Model.Node title desc Nothing time)
        doReturn $ maybe (Left "Create Failed") Right result

    mkrealm S.get "node/:parent" $ do
        parentName  <- S.param "parent"
        parent      <- Lib.runDB (fetchByName parentName)
        doReturn =<< Lib.runDB (DB.selectList [Model.NodeParent ==. (Just . DB.entityKey $ parent)] [] :: DB.SqlPersistT IO [DB.Entity Model.Node])

    mkrealm S.post "node/:parent" $ do
        _           <- justAdminUser
        parentName  <- S.param "parent"
        parent      <- Lib.runDB (fetchByName parentName)
        title       <- loadString "title"
        desc        <- loadString "description"
        time        <- liftIO getCurrentTime
        result      <- Lib.runDB (DB.insertUnique $ Model.Node title desc (Just . DB.entityKey $ parent) time)
        doReturn $ maybe (Left "Create Failed") Right result

    mkrealm S.delete "node/:node" $ do
        nodeName    <- loadString "node"
        _           <- justAdminUser
        _           <- Lib.runDB (DB.deleteCascadeWhere [Model.NodeName ==. nodeName])
        doReturn True

    mkrealm S.put "node/:node" $ let
        checkRec self (Just node)
            | self == Model.nodeParent node = return True
            | otherwise                     = do
                parent <- DB.belongsTo Model.nodeParent node
                checkRec self parent
        checkRec _ Nothing     = return False
        in do
        _           <- justAdminUser
        nodeName    <- loadString "node"
        title       <- loadString "title"
        desc        <- loadString "desc"
        parentName  <- loadString "parent"
        unless (parentName /= nodeName) $ doFinish "Recursive is detected."
        self        <- Lib.runDB (fetchByName nodeName)
        parent      <- Lib.runDB (DB.getBy $ Model.UniqueNodeName parentName)
        hasRec      <- Lib.runDB (checkRec (Just . DB.entityKey $ self) (DB.entityVal <$> parent))
        when hasRec $ doFinish "The node relationship is incorrect."
        doReturn =<< Lib.runDB (Count <$> DB.updateWhereCount [Model.NodeName ==. nodeName] [Model.NodeName =. title, Model.NodeDescription =. desc, Model.NodeParent =. (DB.entityKey <$> parent)])

    mkrealm S.get "messages" $ do
        user    <- justCurrentUser
        doReturn =<< Lib.runDB (DB.selectList [Model.MessageQueueUser ==. DB.toSqlKey (Auth.userId user)] [])

    mkrealm S.put "messages/:id" $ do
        user    <- justCurrentUser
        msgid   <- S.param "id"
        doReturn =<< Lib.runDB (DB.updateWhereCount [Model.MessageQueueUser ==. DB.toSqlKey (Auth.userId user), Model.MessageQueueId ==. DB.toSqlKey msgid] [Model.MessageQueueRead =. True])

    mkrealm S.get "list" $ do
        begin   <- S.param "begin"
        doReturn =<< Lib.runDB (mapM buildExt =<< DB.selectList [] [DB.Desc Model.ArticleEtime, DB.LimitTo 20, DB.OffsetBy begin])

    mkrealm S.get "list/node/:node" $ let
        fetchAllNode (node:xs) = do
            left    <- DB.selectList [Model.NodeParent ==. (Just . DB.entityKey $ node)] []
            right   <- fetchAllNode xs
            return $ node:(left ++ right)
        fetchAllNode [] = return []
        in do
        nodeName    <- loadString "node"
        begin       <- S.param "begin"
        node        <- Lib.runDB (fetchByName nodeName)
        nodes       <- Lib.runDB (fetchAllNode [node])
        doReturn =<< Lib.runDB (mapM buildExt =<< DB.selectList [Model.ArticleNode <-. fmap DB.entityKey nodes] [DB.Desc Model.ArticleEtime, DB.LimitTo 20, DB.OffsetBy begin])

    mkrealm S.get "list/user/:user" $ do
        uname       <- loadString "user"
        begin       <- S.param "begin"
        targetUser  <- Lib.runDB (fetchByName uname)
        doReturn =<< Lib.runDB (mapM buildExt =<< DB.selectList [Model.ArticleAuthor ==. DB.entityKey targetUser] [DB.Desc Model.ArticleEtime, DB.LimitTo 20, DB.OffsetBy begin])

    mkrealm S.get "list/tag/:tag" $ do
        tagName     <- loadString "tag"
        begin       <- S.param "begin"
        targetTag   <- Lib.runDB (fetchByName tagName)
        articleTags <- Lib.runDB (DB.selectList [Model.ArticleTagTag ==. DB.entityKey targetTag] [DB.Desc Model.ArticleTagCtime, DB.LimitTo 20, DB.OffsetBy begin])
        result      <- Lib.runDB (mapM buildExt =<< mapM (DB.getJustEntity . Model.articleTagArticle . DB.entityVal) articleTags)
        doReturn result

    mkrealm S.get "article/:article" $ do
        articleId   <- S.param "article"
        article     <- Lib.runDB (DB.getEntity $ DB.toSqlKey articleId) :: S.ActionT T.Text ConfigM (Maybe (DB.Entity Model.Article))
        doReturn . maybe (Left "Not Found") Right =<< Lib.runDB (mapM buildExt article)

    mkrealm S.delete "article/:article" $ do
        user        <- justCurrentUser
        articleId   <- S.param "article"
        article     <- Lib.runDB (DB.getJust $ DB.toSqlKey articleId)
        unless (Auth.checkAdmin user && DB.fromSqlKey (Model.articleAuthor article) == Auth.userId user) $ doFinish "Permission Denied"
        Lib.runDB (DB.deleteCascade (DB.toSqlKey articleId :: DB.Key Model.Article))
        doReturn True

    mkrealm S.put "article/:article" $ do
        user        <- justCurrentUser
        articleId   <- S.param "article"
        nodeName    <- loadString "node"
        title       <- loadString "title"
        content     <- loadString "content"
        article     <- Lib.runDB (DB.getJust $ DB.toSqlKey articleId)
        node        <- Lib.runDB (fetchByName nodeName)
        when (Auth.checkAdmin user && DB.fromSqlKey (Model.articleAuthor article) == Auth.userId user) $ doFinish "Permission Denied"
        Lib.runDB (DB.update (DB.toSqlKey articleId) [Model.ArticleTitle =. title, Model.ArticleNode =. DB.entityKey node, Model.ArticleContent =. content])
        doReturn True

    mkrealm S.post "article" $ do
        user        <- justCurrentUser
        title       <- loadString "title"
        nodeName    <- loadString "node"
        atype       <- loadString "type"
        content     <- loadString "content"
        node        <- Lib.runDB (fetchByName nodeName)
        time        <- liftIO getCurrentTime
        _           <- Lib.runDB (DB.insert $ Model.Article title (DB.toSqlKey $ Auth.userId user) (DB.entityKey node) (read atype) content time time)
        doReturn True

    mkrealm S.get "article/:article/comment" $ do
        articleId   <- S.param "article"
        begin       <- S.param "begin"
        doReturn =<< Lib.runDB (mapM buildExt =<< DB.selectList [Model.CommentTarget ==. DB.toSqlKey articleId] [DB.Desc Model.CommentCtime, DB.LimitTo 20, DB.OffsetBy begin])

    mkrealm S.post "article/:article/comment" $ do
        user        <- justCurrentUser
        articleId   <- S.param "article"
        content     <- loadString "content"
        time        <- liftIO getCurrentTime
        _           <- Lib.runDB (DB.insert $ Model.Comment (DB.toSqlKey articleId) (DB.toSqlKey $ Auth.userId user) content time)
        doReturn True

    mkrealm S.delete "comment/:comment" $ do
        user        <- justCurrentUser
        commentId   <- S.param "comment"
        comment     <- Lib.runDB (DB.getJust $ DB.toSqlKey commentId)
        when (Auth.checkAdmin user && DB.fromSqlKey (Model.commentAuthor comment) == Auth.userId user) $ doFinish "Permission Denied"
        Lib.runDB (DB.deleteCascade (DB.toSqlKey commentId :: DB.Key Model.Comment))
        doReturn True

    mkrealm S.post "tag" $ do
        _       <- justAdminUser
        _name   <- loadString "name"
        doReturn . maybe (Left "Tag Creating Failed") Right =<< Lib.runDB (DB.insertUnique $ Model.Tag _name)

    mkrealm S.put "tag/:tag" $ do
        _       <- justAdminUser
        tagName <- loadString "tag"
        newName <- loadString "name"
        doReturn =<< Lib.runDB (Count <$> DB.updateWhereCount [Model.TagName ==. tagName] [Model.TagName =. newName])

    mkrealm S.delete "tag/:tag" $ do
        _       <- justAdminUser
        tagName <- loadString "tag"
        doReturn =<< Lib.runDB (Count <$> DB.deleteWhereCount [Model.TagName ==. tagName])

    mkrealm S.post "article/:article/tag/:tag" $ do
        user        <- justCurrentUser
        articleId   <- S.param "article"
        tagName     <- loadString "tag"
        time        <- liftIO getCurrentTime
        tag         <- Lib.runDB (fetchByName tagName)
        doReturn . maybe (Left "Duplicated tag") Right =<< Lib.runDB (DB.insertUnique $ Model.ArticleTag (DB.toSqlKey articleId) (DB.entityKey tag) (DB.toSqlKey $ Auth.userId user) time)

    mkrealm S.delete "article/:article/tag/:tag" $ do
        _           <- justCurrentUser
        articleId   <- S.param "article"
        tagName     <- loadString "tag"
        tag         <- Lib.runDB (fetchByName tagName)
        Lib.runDB (DB.deleteBy $ Model.UniqueArticleTag (DB.toSqlKey articleId) (DB.entityKey tag))
        doReturn True
