{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module WebModule.Favicon where

#if SERVER
import qualified Data.ByteString as B (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text, unpack)
import Happstack.Server (dirs, ok, Response, ServerPartT, setHeader, ToMessage(toResponse))
import Text.Blaze.Html5 ((!), Markup, ToValue(toValue))
import qualified Text.Blaze.Html5 as H (link)
import qualified Text.Blaze.Html5.Attributes as HA (href, rel, type_)
import WebModule.ModuleScopeURL (moduleScopeURL, ModuleScopeURL, URI)

default (Text)


faviconURL :: Text
faviconURL = "/favicon.ico"

faviconURLMS :: ModuleScopeURL
faviconURLMS = $(moduleScopeURL $ unpack "/favicon.ico")

favicon :: B.ByteString
favicon = $(embedFile "server/favicon.ico")

faviconHandler :: B.ByteString -> ServerPartT IO Response
faviconHandler bs = dirs "/favicon.ico" $ ok $ setMimeType $ toResponse bs
  where setMimeType = setHeader "Content-Type" manifestMimeType
        manifestMimeType = "text/cache-manifest"

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
#endif