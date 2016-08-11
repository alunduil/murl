{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
  Module      : Murl.Store.Urls
  Description : A mapping between ShortUrl and LongUrl pairs.
  Copyright   : (c) 2016 murl developers
  License     : MIT
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}
module Murl.Store.Urls where

import Data.Aeson.Types
import Control.Concurrent.STM
import Control.Monad.Catch (MonadThrow)
import qualified Data.Bimap as M
import Data.ByteString.Conversion.To (ToByteString(..))
import GHC.Generics
import Web.HttpApiData (FromHttpApiData(..))

-- | A long URL.
data LongUrl = LongUrl String
  deriving (Eq, Show, FromJSON, ToJSON, Ord, Generic)

instance FromHttpApiData LongUrl where
  parseUrlPiece = Right . LongUrl . unquote . show

instance ToByteString LongUrl where
  builder (LongUrl s) = builder s

-- | A short URL.
data ShortUrl = ShortUrl String
  deriving (Eq, Show, FromJSON, ToJSON, Ord, Generic)

instance FromHttpApiData ShortUrl where
  parseUrlPiece = Right . ShortUrl . unquote . show

unquote :: String -> String
unquote = init . tail

-- | A bidirectional map between ShortUrls and LongUrls.
type UrlMap = TVar (M.Bimap ShortUrl LongUrl)

-- | The empty UrlMap.
empty :: IO UrlMap
empty = newTVarIO M.empty

-- | Insert a (ShortUrl, LongUrl) pair into UrlMap.
storeUrl :: UrlMap -> ShortUrl -> LongUrl -> IO ()
storeUrl s surl lurl = do
                       putStrLn $ "store: " ++ show surl ++ " -> " ++ show lurl
                       atomically $ modifyTVar s (M.insert surl lurl)

-- | Remove ShortUrl (and corresponding LongUrl) from UrlMap.
removeShortUrl :: UrlMap -> ShortUrl -> IO ()
removeShortUrl s surl = atomically $ modifyTVar s (M.delete surl)

-- | Lookup ShortUrl, returning the corresponding LongUrl.
shortToLong :: UrlMap -> ShortUrl -> IO (Maybe LongUrl)
shortToLong s surl = do
                     putStrLn $ "lookup short: " ++ show surl
                     atomically $ M.lookup surl <$> readTVar s

-- | Lookup LongUrl, returning the corresponding ShortUrl.
longToShort :: UrlMap -> LongUrl -> IO (Maybe ShortUrl)
longToShort s lurl = do
                     putStrLn $ "lookup long: " ++ show lurl
                     atomically $ M.lookupR lurl <$> readTVar s
