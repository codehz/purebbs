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
import qualified ModelExt                             as Model hiding (buildExt)
import           Utils

import           AuthMiddleware
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, asks, liftIO,
                                                       runReaderT, unless, when)
import           Control.Monad.Trans.Class            (lift)
import           Data.Aeson                           (ToJSON)
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
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
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

mkapi method name = (method . capture $ "/api/v1/" ++ name) . (`S.rescue` doFinish)
mkrealm method name = (method . capture $ "/api/v1/realm/" ++ name) . (`S.rescue` doFinish)

currentUser :: S.ScottyError e => S.ActionT e ConfigM (Maybe Auth.Payload)
currentUser = do
    req <- S.request
    userKey <- lift (asks userKey)
    return $ getUser userKey req

justCurrentUser :: S.ScottyError e => S.ActionT e ConfigM Auth.Payload
justCurrentUser = do
    vuser <- currentUser
    unless (isJust vuser) $ doFinish "Permission Denied"
    return $ fromJust vuser

justAdminUser :: S.ScottyError e => S.ActionT e ConfigM Auth.Payload
justAdminUser = do
    user <- justCurrentUser
    unless (Auth.checkAdmin user) $ doFinish "Permission Denied"
    return user

class DB.PersistEntity target => FetchByName target where
    uniqueMethod :: String -> DB.Unique target
    fetchByName :: (DB.PersistEntityBackend target ~ DB.BaseBackend backend, DB.PersistUniqueRead backend) => String -> ReaderT backend IO (DB.Entity target)
    fetchByName name = fromJust <$> DB.getBy (uniqueMethod name)

instance FetchByName Model.Node where
    uniqueMethod = Model.UniqueNodeName

instance FetchByName Model.User where
    uniqueMethod = Model.UniqueUsername

instance FetchByName Model.Tag where
    uniqueMethod = Model.UniqueTagName

api :: Config -> S.ScottyT T.Text ConfigM ()
api cfg = let
    authSetting = "test" { authIsProtected = protectedResources }
    in do
    S.middleware $ withSomeHeader
    S.middleware $ logStdoutDev
    S.middleware $ gzip def
    S.middleware $ tokenAuth (userKey cfg) authSetting

    mkapi S.post "register" $ do
        username    <- T.unpack . T.toLower . T.strip <$> S.param "username"
        password    <- S.param "password"
        time        <- liftIO getCurrentTime
        result      <- Lib.runDB (DB.insertUnique $ Model.User username password False time time)
        doReturn $ maybe (Left "Register Failed") Right result

    mkapi S.post "login" $ let
        proc username (Just entity) = return . Right . S8.unpack . Auth.generateToken secret =<< Auth.makePayload (DB.fromSqlKey $ DB.entityKey entity, username, Model.userAdmin $ DB.entityVal entity) expired
        proc _ Nothing = return $ Left "Login Failed"
        check (username, password) = Lib.runDB (DB.selectFirst [Model.UserUsername ==. username, Model.UserPassword ==. password] [])
        in do
        username    <- T.unpack . T.toLower . T.strip <$> S.param "username"
        password    <- S.param "password"
        doReturn =<< S.liftAndCatchIO . proc username =<< check (username, password)

    mkrealm S.get "whoami" $ doReturn =<< justCurrentUser

    mkrealm S.put "user" $ do
        user    <- justCurrentUser
        oldpass <- S.param "oldpass"
        newpass <- S.param "newpass"
        doReturn =<< Lib.runDB (Count <$> DB.updateWhereCount [Model.UserId ==. (DB.toSqlKey $ Auth.userId user), Model.UserPassword ==. oldpass] [Model.UserPassword =. newpass])

    mkrealm S.get "node" $ doReturn =<< Lib.runDB (DB.selectList [Model.NodeParent ==. Nothing] [] :: DB.SqlPersistT IO [DB.Entity Model.Node])

    mkrealm S.post "node" $ do
        user    <- justAdminUser
        title   <- T.unpack . T.toLower . T.strip <$> S.param "title"
        desc    <- T.unpack . T.toLower . T.strip <$> S.param "description"
        time    <- liftIO getCurrentTime
        result  <- Lib.runDB (DB.insertUnique $ Model.Node title desc Nothing time)
        doReturn $ maybe (Left "Create Failed") Right result

    mkrealm S.get "node/:parent" $ do
        parentName  <- S.param "parent"
        parent      <- Lib.runDB (fetchByName parentName)
        doReturn =<< Lib.runDB (DB.selectList [Model.NodeParent ==. (Just . DB.entityKey $ parent)] [] :: DB.SqlPersistT IO [DB.Entity Model.Node])

    mkrealm S.post "node/:parent" $ do
        user        <- justAdminUser
        parentName  <- S.param "parent"
        parent      <- Lib.runDB (fetchByName parentName)
        title       <- T.unpack . T.toLower . T.strip <$> S.param "title"
        desc        <- T.unpack . T.toLower . T.strip <$> S.param "description"
        time        <- liftIO getCurrentTime
        result      <- Lib.runDB (DB.insertUnique $ Model.Node title desc (Just . DB.entityKey $ parent) time)
        doReturn $ maybe (Left "Create Failed") Right result

    mkrealm S.delete "node/:node" $ do
        user        <- justAdminUser
        nodeName    <- S.param "node"
        result      <- Lib.runDB (DB.deleteCascadeWhere $ [Model.NodeName ==. nodeName])
        doReturn True

    mkrealm S.put "node/:node" $ let
        checkRec self (Just node)
            | self == Model.nodeParent node = return True
            | otherwise                     = do
                parent <- DB.belongsTo Model.nodeParent node
                checkRec self parent
        checkRec _ Nothing     = return False
        in do
        user        <- justAdminUser
        nodeName    <- S.param "node"
        title       <- S.param "title"
        desc        <- S.param "desc"
        parentName  <- S.param "parent"
        unless (parentName /= nodeName) $ doFinish "Recursive is detected."
        self        <- Lib.runDB (fetchByName nodeName)
        parent      <- Lib.runDB (DB.getBy $ Model.UniqueNodeName parentName)
        hasRec      <- Lib.runDB (checkRec (Just . DB.entityKey $ self) (DB.entityVal <$> parent))
        when hasRec $ doFinish "The node relationship is incorrect."
        doReturn =<< Lib.runDB (Count <$> DB.updateWhereCount [Model.NodeName ==. nodeName] [Model.NodeName =. title, Model.NodeDescription =. desc, Model.NodeParent =. (DB.entityKey <$> parent)])

    mkrealm S.get "messages" $ do
        user    <- justCurrentUser
        doReturn =<< Lib.runDB (DB.selectList [Model.MessageQueueUser ==. (DB.toSqlKey $ Auth.userId user)] [])

    mkrealm S.put "messages/:id" $ do
        user    <- justCurrentUser
        msgid   <- S.param "id"
        doReturn =<< Lib.runDB (DB.updateWhereCount [Model.MessageQueueUser ==. (DB.toSqlKey $ Auth.userId user), Model.MessageQueueId ==. (DB.toSqlKey msgid)] [Model.MessageQueueRead =. True])

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
        nodeName    <- S.param "node"
        begin       <- S.param "begin"
        node        <- Lib.runDB (fetchByName nodeName)
        nodes       <- Lib.runDB (fetchAllNode [node])
        doReturn =<< Lib.runDB (mapM buildExt =<< DB.selectList [Model.ArticleNode <-. (fmap DB.entityKey nodes)] [DB.Desc Model.ArticleEtime, DB.LimitTo 20, DB.OffsetBy begin])

    mkrealm S.get "list/user/:user" $ do
        userName    <- S.param "user"
        begin       <- S.param "begin"
        targetUser  <- Lib.runDB (fetchByName userName)
        doReturn =<< Lib.runDB (mapM buildExt =<< DB.selectList [Model.ArticleAuthor ==. (DB.entityKey targetUser)] [DB.Desc Model.ArticleEtime, DB.LimitTo 20, DB.OffsetBy begin])

    mkrealm S.get "list/tag/:tag" $ do
        tagName     <- S.param "tag"
        begin       <- S.param "begin"
        targetTag   <- Lib.runDB (fetchByName tagName)
        articleTags <- Lib.runDB (DB.selectList [Model.ArticleTagTag ==. (DB.entityKey targetTag)] [DB.Desc Model.ArticleTagCtime, DB.LimitTo 20, DB.OffsetBy begin])
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
        unless (Auth.checkAdmin user && (DB.fromSqlKey $ Model.articleAuthor article) == Auth.userId user) $ doFinish "Permission Denied"
        Lib.runDB (DB.deleteCascade $ (DB.toSqlKey articleId :: DB.Key Model.Article))
        doReturn True

    mkrealm S.put "article/:article" $ do
        user        <- justCurrentUser
        articleId   <- S.param "article"
        nodeName    <- S.param "node"
        title       <- S.param "title"
        content     <- S.param "content"
        article     <- Lib.runDB (DB.getJust $ DB.toSqlKey articleId)
        node        <- Lib.runDB (fetchByName nodeName)
        when (Auth.checkAdmin user && (DB.fromSqlKey $ Model.articleAuthor article) == Auth.userId user) $ doFinish "Permission Denied"
        Lib.runDB (DB.update (DB.toSqlKey articleId) [Model.ArticleTitle =. title, Model.ArticleNode =. (DB.entityKey node), Model.ArticleContent =. content])
        doReturn True

    mkrealm S.post "article" $ do
        user        <- justCurrentUser
        title       <- S.param "title"
        nodeName    <- S.param "node"
        atype       <- S.param "type"
        content     <- S.param "content"
        node        <- Lib.runDB (fetchByName nodeName)
        time        <- liftIO getCurrentTime
        Lib.runDB (DB.insert $ Model.Article title (DB.toSqlKey $ Auth.userId user) (DB.entityKey node) (read atype) content time time)
        doReturn True

    mkrealm S.get "article/:article/comment" $ do
        articleId   <- S.param "article"
        begin       <- S.param "begin"
        doReturn =<< Lib.runDB (mapM buildExt =<< DB.selectList [Model.CommentTarget ==. (DB.toSqlKey articleId)] [DB.Desc Model.CommentCtime, DB.LimitTo 20, DB.OffsetBy begin])

    mkrealm S.post "article/:article/comment" $ do
        user        <- justCurrentUser
        articleId   <- S.param "article"
        content     <- S.param "content"
        time        <- liftIO getCurrentTime
        Lib.runDB (DB.insert $ Model.Comment (DB.toSqlKey articleId) (DB.toSqlKey $ Auth.userId user) content time)
        doReturn True

    mkrealm S.delete "comment/:comment" $ do
        user        <- justCurrentUser
        commentId   <- S.param "comment"
        comment     <- Lib.runDB (DB.getJust $ DB.toSqlKey commentId)
        when (Auth.checkAdmin user && (DB.fromSqlKey $ Model.commentAuthor comment) == Auth.userId user) $ doFinish "Permission Denied"
        Lib.runDB (DB.deleteCascade $ (DB.toSqlKey commentId :: DB.Key Model.Comment))
        doReturn True

    mkrealm S.post "tag" $ do
        justAdminUser
        name    <- S.param "name"
        doReturn . maybe (Left "Tag Creating Failed") Right =<< Lib.runDB (DB.insertUnique $ Model.Tag name)

    mkrealm S.put "tag/:tag" $ do
        justAdminUser
        tagName <- S.param "tag"
        name    <- S.param "name"
        doReturn =<< Lib.runDB (Count <$> DB.updateWhereCount [Model.TagName ==. tagName] [Model.TagName =. name])

    mkrealm S.delete "tag/:tag" $ do
        justAdminUser
        tagName <- S.param "tag"
        doReturn =<< Lib.runDB (Count <$> DB.deleteWhereCount [Model.TagName ==. tagName])
