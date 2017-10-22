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
                                                       runReaderT, unless)
import           Control.Monad.Trans.Class            (lift)
import qualified Data.ByteString.Lazy.Char8           as S8
import           Data.Maybe
import qualified Data.Text.Lazy                       as T
import           Data.Time.Clock.POSIX
import           Database.Persist.Postgresql          as DB
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

mkapi method name = method . capture $ "/api/v1/" ++ name
mkrealm method name = method . capture $ "/api/v1/realm/" ++ name

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
        username <- T.unpack . T.strip <$> S.param "username"
        password <- S.param "password"
        time <- liftIO getCurrentTime
        result <- Lib.runDB (DB.insertUnique $ Model.User username password time time)
        returnJson $ maybe (Left "Register Failed") Right result

    mkapi S.post "login" $ let
        proc username (Just uid) = return . Right . S8.unpack . Auth.generateToken secret =<< Auth.makePayload (uid, username) expired
        proc _ Nothing = return $ Left "Login Failed"
        check (username, password) = Lib.runDB (selectFirst [Model.UserUsername ==. username, Model.UserPassword ==. password] [] >>= return . fetchId)
        fetchId = fmap $ fromSqlKey . entityKey
        in do
        username <- T.unpack . T.strip <$> S.param "username"
        password <- S.param "password"
        returnJson =<< S.liftAndCatchIO . proc username =<< check (username, password)

    mkrealm S.get "whoami" $ returnJson . Right =<< justCurrentUser
