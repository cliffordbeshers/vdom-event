{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
#if CLIENT
module WebModule.ManifestURL (manifestURL) where
#endif

#if SERVER
module WebModule.ManifestURL (manifestURL) where

import Network.URI (URI)
import WebModule.EmbedURI (embedRelativeURI)

manifestURL :: URI
manifestURL = $(embedRelativeURI "/manifest.appcache")
#endif