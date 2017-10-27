{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module FollowType where

import           Data.Aeson.Types
import           Database.Persist.TH
import           GHC.Generics

data FollowType = Follow_User | Follow_Article | Follow_Node | Follow_Tag
    deriving (Generic, Show, Read, ToJSON, FromJSON, Eq)

derivePersistFieldJSON "FollowType"
