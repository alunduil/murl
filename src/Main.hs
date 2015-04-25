{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative()
import Snap.Core
import Snap.Util.FileServe()
import Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ( ":urlid", urlHandler ) ]

urlHandler :: Snap ()
urlHandler = do
    urlid <- getParam "urlid"
    maybe ( writeBS "must specify an ID" )
          writeBS urlid
