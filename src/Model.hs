{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           Data.Int
import           Data.Time
import           Database.Persist    ()
import           Database.Persist.TH

import           ArticleType
import           FollowType
import           MessageItem

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username    String
    password    String
    admin       Bool
    ctime       UTCTime default=now()
    etime       UTCTime default=now()
    UniqueUsername username
    deriving Show
Follow json
    user            UserId
    followType      FollowType
    followTarget    Int64
    ctime UTCTime default=now()
MessageQueue json
    user    UserId
    sender  Int64
    item    MessageItem
    read    Bool
    ctime   UTCTime default=now()
Node json
    name        String
    description String
    parent      NodeId Maybe
    ctime       UTCTime default=now()
    UniqueNodeName name
    deriving Show
Article json
    title   String
    author  UserId
    node    NodeId
    type    ArticleType
    content String
    ctime   UTCTime default=now()
    etime   UTCTime default=now()
    deriving Show
Comment json
    target  ArticleId
    author  UserId
    content String
    ctime   UTCTime default=now()
    deriving Show
Tag
    name    String
    UniqueTagName name
    deriving Show
ArticleTag json
    article ArticleId
    tag     TagId
    adder   UserId
    ctime   UTCTime default=now()
    UniqueArticleTag article tag
    deriving Show
|]
