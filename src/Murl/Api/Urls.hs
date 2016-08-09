{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

{-|
  Module      : Murl.Api.Urls
  Description : murl URL API specification
  Copyright   : (c) 2016 murl developers
  License     : MIT
  
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Murl.Api.Urls (
  Api,
  server
) where

import Control.Monad.IO.Class
import Data.Aeson.Types
import Data.ByteString.Conversion.To (toByteString')
import qualified Data.Binary as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L
import qualified Data.Hashable as H
import GHC.Generics
import Network.HTTP.Types (hLocation)
import Servant
import qualified Stores

type CreateUrl = "urls" :> QueryParam "long" Stores.LongUrl :> Put '[JSON] Url
type ReadUrl = "urls" :> Capture "short" Stores.ShortUrl :> Get '[JSON] Url
          :<|> "urls" :> Capture "long" Stores.LongUrl :> Get '[JSON] Url
type DeleteUrl = "urls" :> Capture "short" Stores.ShortUrl :> Delete '[JSON] NoContent
type RedirectUrl = Capture "short" Stores.ShortUrl :> (Verb GET 301) '[JSON] (Headers '[Header "Location" Stores.LongUrl] NoContent)

type Api = CreateUrl :<|> ReadUrl :<|> DeleteUrl :<|> RedirectUrl

data Url = Url {
  long :: Stores.LongUrl,
  short :: Stores.ShortUrl
} deriving (Eq, Show, FromJSON, ToJSON, Generic)

server :: Stores.UrlMap -> Server Api
server s = create s
      :<|> Murl.Api.Urls.read s
      :<|> delete s
      :<|> redirect s

create :: Stores.UrlMap -> Server CreateUrl
create s (Just lurl) = do
                       liftIO $ putStrLn $ "PUT /urls/" ++ show lurl
                       liftIO $ Stores.storeUrl s surl lurl
                       return Url { long = lurl, short = surl }
                       where surl = shorten lurl
create s Nothing = throwError err400 { errReasonPhrase = "long query paramter must be passed" }

read :: Stores.UrlMap -> Server ReadUrl
read s = readShort :<|> readLong
         where readShort :: Stores.ShortUrl -> Handler Url
               readShort surl = do
                                liftIO $ putStrLn $ "GET /urls/" ++ show surl
                                lurl <- liftIO $ Stores.shortToLong s surl
                                case lurl of
                                     Just lurl' -> return Url { long = lurl', short = surl }
                                     Nothing -> throwError err404
               readLong :: Stores.LongUrl -> Handler Url
               readLong lurl = do
                               liftIO $ putStrLn $ "GET /urls/" ++ show lurl
                               surl <- liftIO $ Stores.longToShort s lurl
                               case surl of
                                    Just surl' -> return Url { long = lurl, short = surl' }
                                    Nothing -> throwError err404

delete :: Stores.UrlMap -> Server DeleteUrl
delete s surl = do
                liftIO $ putStrLn $ "DELETE /urls/" ++ show surl
                liftIO $ Stores.removeShortUrl s surl >> return undefined

redirect :: Stores.UrlMap -> Server RedirectUrl
redirect s surl = do
                  liftIO $ putStrLn $ "GET /" ++ show surl
                  lurl <- liftIO $ Stores.shortToLong s surl
                  case lurl of
                       Just lurl' -> return $ addHeader lurl' undefined
                       Nothing -> throwError err404

shorten :: Stores.LongUrl -> Stores.ShortUrl
shorten (Stores.LongUrl s) = Stores.ShortUrl . show . B64.encode . L.toStrict . B.encode . H.hash $ s
