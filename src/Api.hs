{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
  Module      : Api.hs
  Description : murl API specification
  Copyright   : (c) 2016 murl developers
  License     : MIT
  
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Api where

import Api.Statuses
import Servant

type MurlApi = StatusApi

murlServer :: Server MurlApi
murlServer = statusesServer

murlApplication :: Application
murlApplication = serve murlApi murlServer

murlApi :: Proxy MurlApi
murlApi = Proxy
