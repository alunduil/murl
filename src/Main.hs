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

import Murl.Api (application)
import Network.Wai.Handler.Warp (run)
import qualified Stores

main :: IO ()
main = do
  s <- Stores.newUrlMap
  run 8000 (application s)
