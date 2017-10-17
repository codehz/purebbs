{-# LANGUAGE OverloadedStrings #-}
module Api
    (
    api
    ) where

import           Auth
import           Lib
import           Utils

import           AuthMiddleware
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, asks,
                                                       runReaderT, unless)
import           Control.Monad.Trans.Class            (lift)
import           Data.Maybe
import qualified Data.Text.Lazy                       as T
import           Network.Wai                          (Middleware,
                                                       mapResponseHeaders,
                                                       modifyResponse, pathInfo)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Web.Scotty                           (capture)
import qualified Web.Scotty.Trans                     as S

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

    mkrealm S.get "whoami" $ returnJson . Right =<< justCurrentUser
