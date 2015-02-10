{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module WebModule.Favicon where

import qualified Data.ByteString as B (ByteString)
#if SERVER
import Data.FileEmbed (embedFile)
#endif
import Data.Text (Text, unpack)
#if SERVER
import Happstack.Server (dirs, ok, Response, ServerPartT, setHeader, ToMessage(toResponse))
#endif
import Text.Blaze.Html5 ((!), Markup, ToValue(toValue))
import qualified Text.Blaze.Html5 as H (link)
import qualified Text.Blaze.Html5.Attributes as HA (href, rel, type_)
import WebModule.ModuleScopeURL (moduleScopeURL, ModuleScopeURL, URI)

default (Text)


faviconURL :: Text
faviconURL = "/favicon.ico"

faviconURLMS :: ModuleScopeURL
faviconURLMS = $(moduleScopeURL $ unpack "/favicon.ico")

#if SERVER
favicon :: B.ByteString
favicon = $(embedFile "embedded/WebModule/Favicon/favicon.ico")
#endif

#if SERVER
faviconHandler :: B.ByteString -> ServerPartT IO Response
faviconHandler bs = dirs "/favicon.ico" $ ok $ setMimeType $ toResponse bs
  where setMimeType = setHeader "Content-Type" manifestMimeType
        manifestMimeType = "text/cache-manifest"
#endif

faviconMarkup :: URI -> Markup
faviconMarkup uri =
  do
    H.link 
      ! HA.rel "shortcut icon" 
      ! HA.href (toValue $ show uri)
      ! HA.type_ "image/x-icon"
    H.link 
      ! HA.rel "icon" 
      ! HA.href (toValue $ show uri)
      ! HA.type_ "image/x-icon"
