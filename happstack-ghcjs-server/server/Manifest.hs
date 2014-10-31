{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

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
import Data.Text as Text (Text, unlines, pack)
import Data.Text.IO as Text (putStr)
import Data.Time
import Data.Time.Clock (getCurrentTime, UTCTime)

import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Network.URI
import WebModule
import ManifestURL (manifestURL)

default (Text)

genManifest :: WS -> Text
genManifest = genManifestChecksum

genManifestChecksum :: WS -> Text
genManifestChecksum ws' = Text.unlines $ header ++ cache ws ++ network ws ++ fallback ws
  where header = ["CACHE MANIFEST"] -- , "# Build Date: " <> buildDate ]
        cache ws = "CACHE:" : [ showURL url <> " # " <> checksum content | w@(WSE url content) <- ws ]
        network ws = "NETWORK:" : ["*"]
        fallback ws = "FALLBACK:" : []
        showURL = Text.pack . show
        checksum = Text.decodeUtf8 . Base16.encode . MD5.hash
        ws = convert ws'

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks
