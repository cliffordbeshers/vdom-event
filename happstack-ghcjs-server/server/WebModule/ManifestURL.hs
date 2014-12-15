{-# LANGUAGE TemplateHaskell #-}
module WebModule.ManifestURL (manifestURL) where

import WebModule.EmbedURI (embedRelativeURI)
import Network.URI

manifestURL :: URI
manifestURL = $(embedRelativeURI "/manifest.appcache")



