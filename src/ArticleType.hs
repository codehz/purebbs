{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module ArticleType where

import           Data.Aeson.Types
import           Database.Persist.TH
import           GHC.Generics

data ArticleType = LinkShare | ImageShare | NormalArticle
    deriving (Generic, Show, Read, ToJSON, FromJSON, Eq)

derivePersistField "ArticleType"
