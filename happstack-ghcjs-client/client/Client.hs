{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

-- import GHCJS.Concurrent
-- import GHCJS.Types
-- import GHCJS.Foreign
-- import GHCJS.Marshal
-- import GHCJS.DOM

import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML)
import GHC.Generics
import ZipTree
import AdminConsole (content)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import JavaScript.JQuery
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Data.Aeson.TH
import Common
import           Data.Default

import Data.Text.Lazy as Text (Text, unpack)

default(Text)


main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    putStrLn $ unpack "Hello, world blaze!"
    foop
    htmlElementSetInnerHTML body $ unpack $ renderHtml content


foop = ajax "/ajax" [] def