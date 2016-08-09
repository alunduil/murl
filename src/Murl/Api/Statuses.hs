{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
  Module      : Murl.Api.Statuses
  Description : murl status API specification
  Copyright   : (c) 2016 murl developers
  License     : MIT
  
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Murl.Api.Statuses where

import Data.Version
import Paths_murl
import Servant

type Api = "statuses" :> (
                  "ping" :> Get '[PlainText] String
             :<|> "version" :> Get '[PlainText] String
           )

server :: Server Api
server = return "pong"
    :<|> return (showVersion version)
