{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Auth (generateToken, makePayload, decodeToken, getValidUser, userId, userName, checkAdmin, Payload(..)) where

import qualified Data.Aeson                  as JSON
import           Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy        as T
import qualified Data.ByteString.Lazy.Char8  as T8
import           Data.Digest.Pure.SHA        as SHA
import           Data.Int
import           Data.Time.Clock.POSIX
import           GHC.Generics

header :: T.ByteString
header = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9."

headerLength :: Integer
headerLength = fromIntegral . T.length $ header

splCh :: T.ByteString
splCh = T8.singleton '.'

data Payload = Payload
    { uid   :: Int64
    , name  :: String
    , admin :: Bool
    , exp   :: Int64
    } deriving (Generic, Show, Ord, Eq)
instance JSON.ToJSON Payload
instance JSON.ToJSONKey Payload
instance JSON.FromJSON Payload

userId :: Payload -> Int64
userId = uid

userName :: Payload -> String
userName = name

checkAdmin :: Payload -> Bool
checkAdmin = admin

now :: IO Int64
now = round <$> getPOSIXTime

timeOffset :: Int64 -> IO Int64
timeOffset = (<$> now) . (+)

generateToken :: T.ByteString -> Payload -> T.ByteString
generateToken secret payload = T.concat [signedPart, splCh, signature] where
    payloadData = Base64.encode $ JSON.encode payload
    signedPart = T.concat [header, payloadData]
    signature = Base64.encode . SHA.bytestringDigest $ SHA.hmacSha256 secret signedPart

makePayload :: (Int64, String, Bool) -> Int64 -> IO Payload
makePayload (id, name, admin) offset = Payload id name admin <$> timeOffset offset

decodeToken :: T.ByteString -> T.ByteString -> Either String Payload
decodeToken secret token = maybe (Left "Not a valid header") tryDecode (T8.stripPrefix header token) where
    bodyPart = T8.takeWhile (/= '.')
    signaturePart = stail . src where
        src = T8.dropWhile (/= '.')
        stail d = if T.null d
            then T.empty
            else T.tail d
    signedPart part = T.concat $ [header, bodyPart part]
    signedPart :: T.ByteString -> T.ByteString
    realSignature part = Base64.encode . SHA.bytestringDigest $ SHA.hmacSha256 secret $ signedPart part
    decodeBody = Base64.decode . bodyPart
    decodeJson = maybe (Left "JSON parse failed") Right . JSON.decode'
    tryDecode part = if signaturePart part == realSignature part
        then decodeJson =<< decodeBody part
        else Left "Signature verification failed"
    tryDecode :: T.ByteString -> Either String Payload

getValidUser :: Either String Payload -> IO (Either String Payload)
getValidUser (Right src@(Payload _ _ _ expired)) = do
    time <- now
    return $ if expired > time
        then Right src
        else Left "Token is expired"
getValidUser (Left e) = return $ Left e
