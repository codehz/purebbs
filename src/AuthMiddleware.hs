{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AuthMiddleware (tokenAuth, AuthSettings, AuthFilter, authRealm, authSecret, authOnNoAuth, authIsProtected, getUser) where

import           Auth
import qualified Data.ByteString         as S
import qualified Data.ByteString.Char8   as S8
import qualified Data.ByteString.Lazy    as SL
import           Data.Either.Combinators
import           Data.String             (IsString (..))
import qualified Data.Vault.Lazy         as V
import           Data.Word8              (isSpace, toLower, _colon)
import           Network.HTTP.Types      (hAuthorization, hContentType,
                                          status401)
import           Network.Wai

type AuthFilter = Request -> IO Bool

tokenAuth :: V.Key Payload -> AuthSettings -> Middleware
tokenAuth key AuthSettings{..} app req sendResponse = do
    isProtected <- authIsProtected req
    result <- if isProtected then check else return $ Right Nothing
    proc result
    where
        proc (Right (Just user)) = app req' sendResponse where
            req' = req { vault = vault' }
            vault' = V.insert key user (vault req)
        proc (Right Nothing) = app req sendResponse
        proc (Left reason) = authOnNoAuth authRealm (S8.pack reason) req sendResponse
        check = case (lookup hAuthorization $ requestHeaders req) >>= extractBearerAuth of
            Nothing -> return $ Left "Need login"
            Just token -> mapRight Just <$> (getValidUser $ decodeToken (SL.fromStrict authSecret) (SL.fromStrict token))

getUser :: V.Key Payload -> Request -> Maybe Payload
getUser = (. vault) . V.lookup

data AuthSettings = AuthSettings
    { authRealm       :: !S.ByteString
    , authSecret      :: !S.ByteString
    , authOnNoAuth    :: !(S.ByteString -> S.ByteString -> Application)
    , authIsProtected :: !AuthFilter
    }

instance IsString AuthSettings where
    fromString s = AuthSettings
        { authRealm = fromString s
        , authSecret = "secret"
        , authOnNoAuth = \realm error _req f -> f $ responseLBS
            status401
            [ (hContentType, "application/json")
            , ("WWW-Authenticate", S.concat $
                [ "Bearer realm=\""
                , realm
                , "\", error=\"invalid_token\", error_description=\""
                , error
                , "\""
                ])
            ]
            "{\"error\": \"Bearer authentication is required\"}"
        , authIsProtected = const $ return True
        }

extractBearerAuth :: S.ByteString -> Maybe S.ByteString
extractBearerAuth bs =
    let (x, y) = S.break isSpace bs
    in if S.map toLower x == "bearer"
        then Just $ S.dropWhile isSpace y
        else Nothing
