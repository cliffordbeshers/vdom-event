{-# LANGUAGE OverloadedStrings #-}
module Favicon where

import Data.Text (Text)
import Text.Blaze.Html5 ((!), Markup, toMarkup, toValue)
import qualified Text.Blaze.Html5 as H (body, docTypeHtml, head, link, meta, title)
import qualified Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, rel, type_)

default (Text)

faviconURL :: Text
faviconURL = "/favicon.ico"

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
