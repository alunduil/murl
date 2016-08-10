{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
  Module      : Murl.Api.Statuses
  Description : Status API implementation.
  Copyright   : (c) 2016 murl developers
  License     : MIT
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable

  A generic status API for a RESTful web service.  Implements general
  information (e.g. up, version) retrieval endpoints.
-}
module Murl.Api.Statuses where

import Data.Version
import Paths_murl
import Servant

-- | Statuses API.
type Api = "statuses" :> (
                  "ping" :> Get '[PlainText] String
             :<|> "version" :> Get '[PlainText] String
           )

-- | Stauses server.
server :: Server Api
server = return "pong"
    :<|> return (showVersion version)
