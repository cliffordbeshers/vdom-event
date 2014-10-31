{-# LANGUAGE TemplateHaskell #-}
module ManifestURL (manifestURL) where

import EmbedURI (embedRelativeURI)
import Network.URI

manifestURL :: URI
manifestURL = $(embedRelativeURI "/manifest.appcache")



