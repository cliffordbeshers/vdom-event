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

-- thetallguy: if you have an aeson FromJSON instance you can combine that with the 
-- FromJSRef instance for an aeson Value to build a FromJSRef instance for it

import Data.Text.Lazy as Text (Text, unpack)

default(String)

foo :: ToJSRef v => v -> IO (JSRef v)
foo v = toJSRef v

main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    putStrLn "Hello, world blaze!"
    foo $ MarshalMe 1 "hello"
    htmlElementSetInnerHTML body $ unpack $ renderHtml content
