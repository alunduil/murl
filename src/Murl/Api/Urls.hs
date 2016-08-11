{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

{-|
  Module      : Murl.Api.Urls
  Description : URL shortening API implementation.
  Copyright   : (c) 2016 murl developers
  License     : MIT
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable

  A basic no-frills URL shortening API implementation.  This API does *not*
  include authentication or other niceties included in shortening services
  such as goo.gl.
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
type ReadUrl = "urls" :> QueryParam "short" Store.ShortUrl :> Get '[JSON] Url
          :<|> "urls" :> QueryParam "long" Store.LongUrl :> Get '[JSON] Url
type DeleteUrl = "urls" :> QueryParam "short" Store.ShortUrl :> Delete '[JSON] NoContent
type RedirectUrl = Capture "short" Store.ShortUrl :> (Verb GET 301) '[JSON] (Headers '[Header "Location" Store.LongUrl] NoContent)

-- | URL shortening API.
type Api = CreateUrl :<|> ReadUrl :<|> DeleteUrl :<|> RedirectUrl

data Url = Url {
  long :: Store.LongUrl,
  short :: Store.ShortUrl
} deriving (Eq, Show, FromJSON, ToJSON, Generic)

-- | URL shortening server.
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
create s Nothing = throwError $ missingQueryParam "long"

read :: Store.UrlMap -> Server ReadUrl
read s = readShort :<|> readLong
         where readShort :: Maybe Store.ShortUrl -> Handler Url
               readShort (Just surl) = do
                                       liftIO $ putStrLn $ "GET /urls/" ++ show surl
                                       liftIO (Store.shortToLong s surl) >>= maybe (throwError err404) (return . flip Url surl)
               readShort Nothing = throwError $ missingQueryParam "short"
               readLong :: Maybe Store.LongUrl -> Handler Url
               readLong (Just lurl) = do
                                      liftIO $ putStrLn $ "GET /urls/" ++ show lurl
                                      liftIO (Store.longToShort s lurl) >>= maybe (throwError err404) (return . Url lurl)
               readLong Nothing = throwError $ missingQueryParam "long"

delete :: Store.UrlMap -> Server DeleteUrl
delete s (Just surl) = do
                       liftIO $ putStrLn $ "DELETE /urls/" ++ show surl
                       liftIO $ Store.removeShortUrl s surl >> return undefined
delete s Nothing = throwError $ missingQueryParam "short"

redirect :: Store.UrlMap -> Server RedirectUrl
redirect s surl = do
                  liftIO $ putStrLn $ "GET /" ++ show surl
                  lurl <- liftIO $ Store.shortToLong s surl
                  liftIO $ putStrLn $ "-> " ++ show lurl
                  case lurl of
                       Just lurl' -> return $ addHeader lurl' undefined
                       Nothing -> throwError err404

-- | Shorten a LongUrl into a ShortUrl.
shorten :: Store.LongUrl -> Store.ShortUrl
shorten (Store.LongUrl s) = Store.ShortUrl . init . tail . show . B64.encode . L.toStrict . B.encode . H.hash $ s

missingQueryParam :: String -> ServantErr
missingQueryParam p = err400 { errReasonPhrase = p ++ " query parameter must be passed" }
