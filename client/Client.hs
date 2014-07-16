{- LANGUAGE OverloadedStrings -}
module Main where

-- import GHCJS.Concurrent
-- import GHCJS.Types
-- import GHCJS.Foreign
-- import GHCJS.Marshal
-- import GHCJS.DOM

import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML)
import ZipTree
import AdminConsole (content)

import Data.Text.Lazy as Text (Text, unpack)

default(String)

main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    putStrLn "Hello, world blaze!"
    htmlElementSetInnerHTML body $ unpack $ renderHtml content
