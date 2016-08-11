{-# LANGUAGE TemplateHaskell #-}

{-|
  Module      : Tests.Murl.Api.Urls
  Description : murl URL API tests
  Copyright   : (c) 2016 murl developers
  License     : MIT

  Maintainer  : alunduil@alunduil.com
  Stability   : experimental
  Portability : portable
-}

module Tests.Murl.Api.Urls where

import Control.Monad
import Murl.Api.Urls
import qualified Murl.Store.Urls as Store
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck

instance Arbitrary Store.LongUrl where
  arbitrary = liftM Store.LongUrl arbitrary

prop_shorten_fixed_length x = 12 == (length . fromShort . shorten $ x)

prop_shorten_unquoted x = not . quoted . fromShort . shorten $ x
                          where quoted :: String -> Bool
                                quoted ('"':cs) | last cs == '"' = True
                                quoted ('\'':cs) | last cs == '\'' = True
                                quoted _ = False

fromShort :: Store.ShortUrl -> String
fromShort (Store.ShortUrl s) = s

tests = $(testGroupGenerator)
