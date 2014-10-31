{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Favicon where

import Data.Text (Text, unpack)
import Text.Blaze.Html5 ((!), Markup, toMarkup, toValue)
import qualified Text.Blaze.Html5 as H (body, docTypeHtml, head, link, meta, title)
import qualified Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, rel, type_)
import qualified Data.ByteString as B
import Data.FileEmbed
import Happstack.Server
import ModuleScopeURL

default (Text)

faviconURL :: Text
faviconURL = "/favicon.ico"

faviconURLMS :: ModuleScopeURL
faviconURLMS = $(moduleScopeURL $ unpack "/favicon.ico")

favicon :: B.ByteString
favicon = $(embedFile "server/favicon.ico")

faviconHandler :: B.ByteString -> ServerPartT IO Response
faviconHandler favicon = dirs "/favicon.ico" $ ok $ setMimeType $ toResponse favicon
  where setMimeType = setHeader "Content-Type" manifestMimeType
        manifestMimeType = "text/cache-manifest"


faviconMarkup :: Markup
faviconMarkup =
  do
    H.link 
      ! HA.rel "shortcut icon" 
      ! HA.href (toValue faviconURL)
      ! HA.type_ "image/x-icon"
    H.link 
      ! HA.rel "icon" 
      ! HA.href (toValue faviconURL)
      ! HA.type_ "image/x-icon"
