{- |

Module      : UrlMap.hs
Description : murl URL mapping
Copyright   : (c) 2016 murl developers
License     : MIT

Maintainer  : alunduil@alunduil.com
Stability   : experimental
Portability : portable

-}

module UrlMap where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import qualified Data.Map.Strict        as Map
import           GHC.Conc

newUrlMap :: IO (TVar (Map.Map String String))
newUrlMap = newTVarIO Map.empty

insertUrl :: MonadIO m => String -> String -> TVar (Map.Map String String) -> m (Map.Map String String)
insertUrl s l urls = liftIO . atomically $ do
  writeTVar $ urls Map.insert s l (readTVar urls)
  return urls

lookupUrl :: MonadIO m => String -> TVar (Map.Map String String) -> m (Map.Map String String)
lookupUrl s urls = liftIO (readTVarIO urls) Map.! s
