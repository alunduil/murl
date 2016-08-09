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
  server,
  shorten
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
import qualified Murl.Store.Urls as Store

type CreateUrl = "urls" :> QueryParam "long" Store.LongUrl :> Put '[JSON] Url
type ReadUrl = "urls" :> Capture "short" Store.ShortUrl :> Get '[JSON] Url
          :<|> "urls" :> Capture "long" Store.LongUrl :> Get '[JSON] Url
type DeleteUrl = "urls" :> Capture "short" Store.ShortUrl :> Delete '[JSON] NoContent
type RedirectUrl = Capture "short" Store.ShortUrl :> (Verb GET 301) '[JSON] (Headers '[Header "Location" Store.LongUrl] NoContent)

type Api = CreateUrl :<|> ReadUrl :<|> DeleteUrl :<|> RedirectUrl

data Url = Url {
  long :: Store.LongUrl,
  short :: Store.ShortUrl
} deriving (Eq, Show, FromJSON, ToJSON, Generic)

server :: Store.UrlMap -> Server Api
server s = create s
      :<|> Murl.Api.Urls.read s
      :<|> delete s
      :<|> redirect s

create :: Store.UrlMap -> Server CreateUrl
create s (Just lurl) = do
                       liftIO $ putStrLn $ "PUT /urls/" ++ show lurl
                       liftIO $ Store.storeUrl s surl lurl
                       return Url { long = lurl, short = surl }
                       where surl = shorten lurl
create s Nothing = throwError err400 { errReasonPhrase = "long query paramter must be passed" }

read :: Store.UrlMap -> Server ReadUrl
read s = readShort :<|> readLong
         where readShort :: Store.ShortUrl -> Handler Url
               readShort surl = do
                                liftIO $ putStrLn $ "GET /urls/" ++ show surl
                                liftIO (Store.shortToLong s surl) >>= maybe (throwError err404) (return . flip Url surl)
               readLong :: Store.LongUrl -> Handler Url
               readLong lurl = do
                               liftIO $ putStrLn $ "GET /urls/" ++ show lurl
                               liftIO (Store.longToShort s lurl) >>= maybe (throwError err404) (return . Url lurl)

delete :: Store.UrlMap -> Server DeleteUrl
delete s surl = do
                liftIO $ putStrLn $ "DELETE /urls/" ++ show surl
                liftIO $ Store.removeShortUrl s surl >> return undefined

redirect :: Store.UrlMap -> Server RedirectUrl
redirect s surl = do
                  liftIO $ putStrLn $ "GET /" ++ show surl
                  lurl <- liftIO $ Store.shortToLong s surl
                  case lurl of
                       Just lurl' -> return $ addHeader lurl' undefined
                       Nothing -> throwError err404

shorten :: Store.LongUrl -> Store.ShortUrl
shorten (Store.LongUrl s) = Store.ShortUrl . init . tail . show . B64.encode . L.toStrict . B.encode . H.hash $ s
