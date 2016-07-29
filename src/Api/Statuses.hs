{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
  Module      : Api.Statuses
  Description : murl status API specification
  Copyright   : (c) 2016 murl developers
  License     : MIT
  
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Api.Statuses where

import Servant

type StatusApi = "statuses" :> "ping" :> Get '[PlainText] String

statusesServer :: Server StatusApi
statusesServer = return "pong"
