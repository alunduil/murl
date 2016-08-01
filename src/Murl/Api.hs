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
import Servant

application :: Application
application = serve api server

type Api = Statuses.Api

api :: Proxy Api
api = Proxy

server :: Server Api
server = Statuses.server
