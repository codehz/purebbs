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

import           Data.Time
import           Database.Persist    ()
import           Database.Persist.TH

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username String
    password String
    ctime UTCTime default=now()
    etime UTCTime default=now()
    UniqueUsername username
    deriving Show
Node json
    name String
    description String
    parent NodeId Maybe
    ctime UTCTime default=now()
    UniqueNodeName name
    deriving Show
Article json
    title String
    authorId UserId
    node NodeId
    content String
    ctime UTCTime default=now()
    etime UTCTime default=now()
    deriving Show
Comment json
    target ArticleId
    authorId UserId
    content String
    ctime UTCTime default=now()
    deriving Show
Tag
    name String
    UniqueName name
    deriving Show
ArticleTag
    article ArticleId
    tag TagId
    adder UserId
    deriving Show
|]
