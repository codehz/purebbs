{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
module Utils(DoReturn(doReturn, doFinish), Count(..)) where
import           Data.Aeson       (ToJSON, Value (..), object, (.=))
import           Data.Int
import           Data.Text.Lazy   (Text)
import qualified Web.Scotty       ()
import           Web.Scotty.Trans

default(Text)

newtype Count = Count Int64 deriving ToJSON

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

instance {-# OVERLAPPING #-} DoReturn Count where
    doConvert (Count 0) = object ["error" .= "Nothing is updated"]
    doConvert _         = object ["result" .= True ]

instance {-# OVERLAPPING #-} (ToJSON a, ToJSON b) => DoReturn (Either a b) where
    doConvert = object . (:[]) . either ("error" .=) ("result" .=)

instance (ToJSON a) => DoReturn a where
