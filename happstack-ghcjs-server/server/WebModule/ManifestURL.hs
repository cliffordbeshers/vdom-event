{-# LANGUAGE TemplateHaskell #-}
module WebModule.ManifestURL (manifestURL) where

import Network.URI (URI)
import WebModule.EmbedURI (embedRelativeURI)

manifestURL :: URI
manifestURL = $(embedRelativeURI "/manifest.appcache")



