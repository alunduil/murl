{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
  Module      : Murl.Api.hs
  Description : murl API specification
  Copyright   : (c) 2016 murl developers
  License     : MIT
  
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Murl.Api (application) where

import qualified Murl.Api.Statuses as Statuses
import qualified Murl.Api.Urls as Urls
import Servant
import qualified Stores

application :: Stores.UrlMap -> Application
application s = serve api (server s)

type Api = Statuses.Api
      :<|> Urls.Api

api :: Proxy Api
api = Proxy

server :: Stores.UrlMap -> Server Api
server s = Statuses.server
      :<|> Urls.server s
