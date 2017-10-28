{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( Config(..)
    , ConfigM(..)
    , Lib.init
    , runDB
    ) where

import qualified Auth
import           Control.Applicative         (Applicative)
import           Control.Monad               (Functor, Monad)
import           Control.Monad.Logger        (runStdoutLoggingT)
import           Control.Monad.Reader        (MonadIO, MonadReader, ReaderT,
                                              asks)
import           Control.Monad.Trans.Class   (lift)
import qualified Data.ByteString.Char8       as BS8
import           Data.Time.Clock.POSIX
import qualified Data.Vault.Lazy             as V
import qualified Database.Persist.Postgresql as DB
import qualified Model
import           Web.Scotty                  ()
import qualified Web.Scotty.Trans            as S

data Config = Config {connectionPool :: DB.ConnectionPool, userKey :: V.Key Auth.Payload}

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

doMigrations :: ReaderT DB.SqlBackend IO ()
doMigrations = DB.runMigration Model.migrateAll

init :: String -> IO Config
init constr = do
    pool <- runStdoutLoggingT $ DB.createPostgresqlPool (BS8.pack constr) 10
    DB.runSqlPool doMigrations pool
    time <- getCurrentTime
    DB.runSqlPool (DB.insertUnique $ Model.User "admin" "admin" True time time) pool
    userKey <- V.newKey
    return $ Config {connectionPool = pool, userKey = userKey}

runDB :: S.ScottyError e => DB.SqlPersistT IO a -> S.ActionT e ConfigM a
runDB query = lift (asks connectionPool) >>= (S.liftAndCatchIO . DB.runSqlPool query)
