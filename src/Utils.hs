{-# LANGUAGE OverloadedStrings #-}

module Utils where
import           Data.Aeson       (ToJSON, Value (..), object, (.=))
import           Data.Text        (Text, empty, unpack)
import qualified Web.Scotty       ()
import           Web.Scotty.Trans

convertJson :: (ToJSON a) => Either String a -> Value
convertJson = object . (:[]) . either ("error" .=) ("result" .=)

returnJson :: (ScottyError e, Monad m, ToJSON a) => Either String a -> ActionT e m ()
returnJson = json . convertJson

returnError :: (ScottyError e, Monad m) => String -> ActionT e m ()
returnError e = returnJson param where
    param = Left e
    param :: Either String String

finishJson :: (ScottyError e, Monad m, ToJSON a) => Either String a -> ActionT e m ()
finishJson t = do
    returnJson t
    finish

finishError :: (ScottyError e, Monad m) => String -> ActionT e m ()
finishError e = finishJson param where
    param = Left e
    param :: Either String String
