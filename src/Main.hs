{-|
  Module      : Main.hs
  Description : murl Main execution
  Copyright   : (c) 2016 murl developers
  License     : MIT
  
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Main where

import Api
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = run 8000 murlApplication
