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
import qualified Murl.Store.Urls as Store

main :: IO ()
main = do
  s <- Store.newUrlMap
  run 8000 (application s)
