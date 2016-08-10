{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
  Module      : Murl.Api
  Description : murl API implementation.
  Copyright   : (c) 2016 murl developers
  License     : MIT
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable

  The composed and complete API for murl.
-}
module Murl.Api (application) where

import qualified Murl.Api.Statuses as Statuses
import qualified Murl.Api.Urls as Urls
import Servant
import qualified Murl.Store.Urls as Store

-- | murl API application.
application :: Store.UrlMap -> Application
application s = serve api (server s)

type Api = Statuses.Api
      :<|> Urls.Api

api :: Proxy Api
api = Proxy

server :: Store.UrlMap -> Server Api
server s = Statuses.server
      :<|> Urls.server s
