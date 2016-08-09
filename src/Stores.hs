{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
  Module      : Stores.hs
  Description : murl Stores
  Copyright   : (c) 2016 murl developers
  License     : MIT
  
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Stores where

import Data.Aeson.Types
import Control.Concurrent.STM
import Control.Monad.Catch (MonadThrow)
import qualified Data.Bimap as M
import Data.ByteString.Conversion.To (ToByteString(..))
import GHC.Generics
import Web.HttpApiData (FromHttpApiData(..))

data LongUrl = LongUrl String
  deriving (Eq, Show, FromJSON, ToJSON, Ord, Generic)

instance FromHttpApiData LongUrl where
  parseUrlPiece = Right . LongUrl . show

instance ToByteString LongUrl where
  builder (LongUrl s) = builder s

data ShortUrl = ShortUrl String
  deriving (Eq, Show, FromJSON, ToJSON, Ord, Generic)

instance FromHttpApiData ShortUrl where
  parseUrlPiece = Right . ShortUrl . show

type UrlMap = TVar (M.Bimap ShortUrl LongUrl)

newUrlMap :: IO UrlMap
newUrlMap = newTVarIO M.empty

storeUrl :: UrlMap -> ShortUrl -> LongUrl -> IO ()
storeUrl s surl lurl = atomically $ modifyTVar s (M.insert surl lurl)

removeShortUrl :: UrlMap -> ShortUrl -> IO ()
removeShortUrl s surl = atomically $ modifyTVar s (M.delete surl)

shortToLong :: UrlMap -> ShortUrl -> IO (Maybe LongUrl)
shortToLong s surl = atomically $ M.lookup surl <$> readTVar s

longToShort :: UrlMap -> LongUrl -> IO (Maybe ShortUrl)
longToShort s lurl = atomically $ M.lookupR lurl <$> readTVar s
