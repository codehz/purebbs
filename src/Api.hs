{-# LANGUAGE OverloadedStrings #-}
module Api
    (
    api
    ) where

import           Auth
import           Lib
import qualified Model
import           Utils

import           AuthMiddleware
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, asks, liftIO,
                                                       runReaderT, unless, when)
import           Control.Monad.Trans.Class            (lift)
import qualified Data.ByteString.Lazy.Char8           as S8
import           Data.Maybe
import qualified Data.Text.Lazy                       as T
import           Data.Time.Clock.POSIX
import           Database.Persist.Postgresql          ((=.), (==.))
import qualified Database.Persist.Postgresql          as DB
import           Network.Wai                          (Middleware,
                                                       mapResponseHeaders,
                                                       modifyResponse, pathInfo)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Web.Scotty                           (capture)
import qualified Web.Scotty.Trans                     as S

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

mkapi method name = (method . capture $ "/api/v1/" ++ name) . (`S.rescue` returnError)
mkrealm method name = (method . capture $ "/api/v1/realm/" ++ name) . (`S.rescue` returnError)

currentUser :: S.ScottyError e => S.ActionT e ConfigM (Maybe Auth.Payload)
currentUser = do
    req <- S.request
    userKey <- lift (asks userKey)
    return $ getUser userKey req

justCurrentUser :: S.ScottyError e => S.ActionT e ConfigM Auth.Payload
justCurrentUser = do
    vuser <- currentUser
    unless (isJust vuser) $ finishError "Permission Denied"
    return $ fromJust vuser

api :: Config -> S.ScottyT T.Text ConfigM ()
api cfg = do
    let authSetting = "test" { authIsProtected = protectedResources }
    S.middleware $ withSomeHeader
    S.middleware $ logStdoutDev
    S.middleware $ gzip def
    S.middleware $ tokenAuth (userKey cfg) authSetting

    mkapi S.post "register" $ do
        username <- T.unpack . T.toLower . T.strip <$> S.param "username"
        password <- S.param "password"
        time <- liftIO getCurrentTime
        result <- Lib.runDB (DB.insertUnique $ Model.User username password False time time)
        returnJson $ maybe (Left "Register Failed") Right result

    mkapi S.post "login" $ let
        proc username (Just entity) = return . Right . S8.unpack . Auth.generateToken secret =<< Auth.makePayload (DB.fromSqlKey $ DB.entityKey entity, username, Model.userAdmin $ DB.entityVal entity) expired
        proc _ Nothing = return $ Left "Login Failed"
        check (username, password) = Lib.runDB (DB.selectFirst [Model.UserUsername ==. username, Model.UserPassword ==. password] [])
        in do
        username <- T.unpack . T.toLower . T.strip <$> S.param "username"
        password <- S.param "password"
        returnJson =<< S.liftAndCatchIO . proc username =<< check (username, password)

    mkrealm S.get "whoami" $ returnJson . Right =<< justCurrentUser

    mkrealm S.put "user" $ do
        user <- justCurrentUser
        oldpass <- S.param "oldpass"
        newpass <- S.param "newpass"
        result <- Lib.runDB (DB.updateWhereCount [Model.UserId ==. (DB.toSqlKey $ Auth.userId user), Model.UserPassword ==. oldpass] [Model.UserPassword =. newpass])
        returnJson $ Right result

    mkrealm S.get "node" $ returnJson . Right =<< Lib.runDB (DB.selectList [Model.NodeParent ==. Nothing] [] :: DB.SqlPersistT IO [DB.Entity Model.Node])

    mkrealm S.post "node" $ do
        user <- justCurrentUser
        unless (Auth.checkAdmin user) $ finishError "Permission denied."
        title <- T.unpack . T.toLower . T.strip <$> S.param "title"
        desc <- T.unpack . T.toLower . T.strip <$> S.param "description"
        time <- liftIO getCurrentTime
        result <- Lib.runDB (DB.insertUnique $ Model.Node title desc Nothing time)
        returnJson $ maybe (Left "Create Failed") Right result

    mkrealm S.get "node/:parent" $ do
        parentName <- S.param "parent"
        parent <- Lib.runDB (DB.getBy $ Model.UniqueNodeName parentName)
        when (isNothing parent) $ finishError "Node is not exist."
        returnJson . Right =<< Lib.runDB (DB.selectList [Model.NodeParent ==. (DB.entityKey <$> parent)] [] :: DB.SqlPersistT IO [DB.Entity Model.Node])

    mkrealm S.post "node/:parent" $ do
        user <- justCurrentUser
        unless (Auth.checkAdmin user) $ finishError "Permission denied."
        parentName <- S.param "parent"
        parent <- Lib.runDB (DB.getBy $ Model.UniqueNodeName parentName)
        when (isNothing parent) $ finishError "Node is not exist."
        title <- T.unpack . T.toLower . T.strip <$> S.param "title"
        desc <- T.unpack . T.toLower . T.strip <$> S.param "description"
        time <- liftIO getCurrentTime
        result <- Lib.runDB (DB.insertUnique $ Model.Node title desc (DB.entityKey <$> parent) time)
        returnJson $ maybe (Left "Create Failed") Right result

    mkrealm S.get "messages" $ do
        user <- justCurrentUser
        returnJson . Right =<< Lib.runDB (DB.selectList [Model.MessageQueueUser ==. (DB.toSqlKey $ Auth.userId user)] [])
