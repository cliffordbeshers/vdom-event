{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module WebModule where

import Data.Text as T
import "network-uri" Network.URI
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA (href, rel, src, type_)
import Text.Blaze.Html.Renderer.Utf8  (renderHtml)
import Data.ByteString as B -- Force Strict

default (T.Text)

data WebModule = WebModule [WebImport]

data MimeType = MT_CSS | MT_Javascript

data WebImport = WI MimeType URI B.ByteString

instance ToMarkup WebImport where
  toMarkup (WI MT_CSS uri content) =   
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href (toValue . show $ uri)
  toMarkup (WI MT_Javascript uri content) =   
    H.script ! HA.type_ "text/javascript" ! HA.src (toValue . show $ uri) $ return ()

