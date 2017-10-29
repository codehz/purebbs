{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Utils(doReturn, doFinish) where
import           Data.Aeson       (ToJSON, Value (..), object, (.=))
import           Data.Text.Lazy   (Text)
import qualified Web.Scotty       ()
import           Web.Scotty.Trans

class ToJSON a => DoReturn a where
    doConvert :: a -> Value
    doConvert = object . (:[]) . ("result" .=)
    doReturn :: (ScottyError e, Monad m) => a -> ActionT e m ()
    doReturn = json . doConvert
    doFinish :: (ScottyError e, Monad m) => a -> ActionT e m ()
    doFinish x = do
        doReturn x
        finish

instance {-# OVERLAPPING #-} DoReturn Text where
    doConvert = object . (:[]) . ("error" .=)

instance {-# OVERLAPPING #-} (ToJSON a, ToJSON b) => DoReturn (Either a b) where
    doConvert = object . (:[]) . either ("error" .=) ("result" .=)

instance (ToJSON a) => DoReturn a where
