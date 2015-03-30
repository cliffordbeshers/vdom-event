{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Bootstrap where

import Data.Text as Text (Text)
import GHCJS.Foreign.QQ
import GHCJS.Types (JSString)
import GHCJS.Foreign (toJSString)
import GHCJS.VDOM as VDOM (DOMNode)

inlineCSS :: Text -> IO DOMNode
inlineCSS csst = let css = toJSString csst in
  do
    node  <- [js| document.createElement('style') |]
    [js_| (`node).innerText = (`css); |]
    [js_| document.head.appendChild(`node); |]
    return node

externalScript :: JSString -> IO DOMNode
externalScript url = do
  node  <- [js| document.createElement('script') |]
  [js_| `node.setAttribute('language','javascript'); |]
  [js_| `node.setAttribute('src',`url); |]
  [js_| document.head.appendChild(`node); |]
  return node

externalCSS :: JSString -> IO DOMNode
externalCSS url = do
  node  <- [js| document.createElement('link') |]
  [js_| `node.setAttribute('rel','stylesheet'); |]
  [js_| `node.setAttribute('href',`url); |]
  [js_| document.head.appendChild(`node); |]
  return node

loadJQuery :: IO DOMNode
loadJQuery = externalScript "https://code.jquery.com/jquery-1.11.2.min.js"

loadBootstrap :: IO [DOMNode]
loadBootstrap = do
  jqjs <- loadJQuery
  bjs <- externalScript "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"
  bcss <- externalCSS "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
  return [jqjs,bjs,bcss]
