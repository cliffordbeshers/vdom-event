{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Generate an html5 manifest file, a list of all resources
-- to be cached (or not) for offline 
module Manifest where

import Crypto.Hash.MD5 as MD5 (hash)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as BL

import Data.ByteString.Base16 as Base16 (encode)
import Data.Text.Encoding as Text (decodeUtf8)

import Data.Monoid ((<>))
import Data.Text as Text
import Data.Text.IO as Text (putStr)
import Data.Time
import Data.Time.Clock (getCurrentTime, UTCTime)

import Language.Haskell.TH
import Language.Haskell.TH.Lift
import "network-uri" Network.URI
import WebModule

default (Text)


type WS = WebModule
data WSE = WSE URI B.ByteString | WSN URI | WS_FALLBACK URI URI

convert :: WebModule -> WS
convert (WebModule wis) = map cv wis
  where cv (WI _ uri content) = WSE uri content

manifestURL :: URI
manifestURL = "/manifest.appcache"

genManifest :: WS -> Text
genManifest = genManifestChecksum

genManifest :: WS -> Text
genManifest = genManifestChecksum

genManifest' :: WS -> Text
genManifest' ws = Text.unlines $ header ++ cache ws ++ network ws ++ fallback ws
  where header = ["CACHE MANIFEST", "# Build Date: " <> buildDate ]
        cache ws = "CACHE:" : [ url | WSE url content <- ws ]
        network ws = "NETWORK:" : ["*"]
        fallback ws = "FALLBACK:" : []

genManifestChecksum :: WS -> Text
genManifestChecksum ws = Text.unlines $ header ++ cache ws ++ network ws ++ fallback ws
  where header = ["CACHE MANIFEST", "# Build Date: " <> buildDate ]
        cache ws = "CACHE:" : [ showURL url <> " # " <> checksum content | w@(WSE url content) <- ws ]
        network ws = "NETWORK:" : ["*"]
        fallback ws = "FALLBACK:" : []
        showURL = id
        checksum = Text.decodeUtf8 . Base16.encode . MD5.hash

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks
