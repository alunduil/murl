{-|
  Module      : test-murl.hs
  Description : murl Main test
  Copyright   : (c) 2016 murl developers
  License     : MIT
  
  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Main where

import GHC.IO.Encoding
import Test.Framework
import qualified Tests.Murl.Api.Urls
import System.Environment (getArgs)

tests :: [Test]
tests = [Tests.Murl.Api.Urls.tests]

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  defaultMainWithArgs tests args
