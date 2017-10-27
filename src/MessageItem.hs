{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MessageItem where

import           Data.Aeson.Types
import           Data.Int
import           Database.Persist.TH
import           GHC.Generics

data MsgCreateArticleSrc = MsgSrcUser | MsgSrcNode | MsgSrcTag
    deriving (Generic, Show, Read, ToJSON, FromJSON, Eq)

data MessageItem
    = MsgCreateArticle { src :: MsgCreateArticleSrc, articleId :: Int64 }
    | MsgCommentArtucle { commentId :: Int64 }
    | MsgPrivateLetter { content :: String }
    deriving (Generic, Show, Read, ToJSON, FromJSON, Eq)

derivePersistFieldJSON "MessageItem"
