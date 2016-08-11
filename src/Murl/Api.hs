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
module Murl.Api ( api
                , application
                ) where

import qualified Murl.Api.Statuses as Statuses
import qualified Murl.Api.Urls as Urls
import qualified Murl.Store.Urls as Store
import Servant

-- | murl API application.
application :: Store.UrlMap -> Application
application s = serve api (server s)

-- | murl API proxy object (used for servant derivations).
type Api = Statuses.Api
      :<|> Urls.Api

api :: Proxy Api
api = Proxy

server :: Store.UrlMap -> Server Api
server s = Statuses.server
      :<|> Urls.server s
