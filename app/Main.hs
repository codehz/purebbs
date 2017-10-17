module Main where

import           Api
import           Lib

import           Control.Monad.Reader (runReaderT)
import           Web.Scotty           ()
import qualified Web.Scotty.Trans     as S
main :: IO ()
main = do
    config <- Lib.init "host=localhost dbname=bbs user=bbs password=bbs"
    let r m = runReaderT (runConfigM m) config
    S.scottyT 3000 r $ api config
