{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Generate an html5 manifest file, a list of all resources
-- to be cached (or not) for offline 
module Manifest where

import Prelude (($), (++))
import Data.Text as Text
import Data.Text.IO as Text (putStr)

default (Text)

type URI = Text


type WS = [WSE]
data WSE = WSE URI | WSN URI | WS_FALLBACK URI URI


genManifest :: WS -> Text
genManifest ws = Text.unlines $ header ++ cache ws ++ network ws ++ fallback ws
  where header = ["CACHE MANIFEST"]
        cache ws = "CACHE:" : [ url | WSE url <- ws ]
        network ws = "NETWORK:" : ["*"]
        fallback ws = "FALLBACK:" : []

