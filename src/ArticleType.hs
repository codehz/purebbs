{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module ArticleType where

import           Data.Aeson.Types
import           Database.Persist.TH
import           GHC.Generics

data ArticleType = LinkShare | ImageShare | NormalArticle
    deriving (Generic, Show, Read, Eq)

instance ToJSON ArticleType where
instance FromJSON ArticleType

derivePersistField "ArticleType"
