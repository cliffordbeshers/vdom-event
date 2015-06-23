{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Bootstrap where

import Data.Text as Text (Text)
import GHCJS.Types (JSString)
import GHCJS.Foreign (toJSString)

import Alderon.Html
import Alderon.Html.Attributes hiding (style_)

import MicroDOM
import BuildDOM

default (Text)

-- inlineCSSQQ :: Text -> IO DOMNode
-- inlineCSSQQ csst = let css = toJSString csst in
--   do
--     node  <- [js| document.createElement('style') |]
--     [js_| (`node).innerText = (`css); |]
--     [js_| document.head.appendChild(`node); |]
--     return node

inlineCSS :: Text -> IO [DOMElement]
inlineCSS csst = do
  nodes <- buildDOM (style_ $ text_ csst)
  head <- documentHead
  appendChildren head nodes
  return nodes

-- externalScript :: JSString -> IO DOMNode
-- externalScript url = do
--   node  <- [js| document.createElement('script') |]
--   [js_| `node.setAttribute('language','javascript'); |]
--   [js_| `node.setAttribute('src',`url); |]
--   [js_| document.head.appendChild(`node); |]
--   return node

appendHeaders :: Html -> IO [DOMElement]
appendHeaders dom = do
  nodes <- buildDOM dom
  head <- documentHead
  appendChildren head nodes
  return nodes

externalScript :: Text -> IO [DOMElement]
externalScript url = appendHeaders dom
  where dom = script_ 
              ! type_ "text/javascript"
              ! src_ url
              $ text_ ""

-- externalCSS :: JSString -> IO DOMNode
-- externalCSS url = do
--   node  <- [js| document.createElement('link') |]
--   [js_| `node.setAttribute('rel','stylesheet'); |]
--   [js_| `node.setAttribute('href',`url); |]
--   [js_| document.head.appendChild(`node); |]
--   return node

externalCSS :: Text -> IO [DOMElement]
externalCSS url = appendHeaders dom
  where dom = link_
              ! rel_ "stylesheet"
              ! href_ url

-- loadJQuery :: IO DOMNode
-- loadJQuery = externalScript "https://code.jquery.com/jquery-1.11.2.min.js"

loadJQuery :: IO [DOMElement]
loadJQuery = externalScript "https://code.jquery.com/jquery-1.11.2.min.js"

-- loadBootstrap :: IO [DOMNode]
-- loadBootstrap = do
--   jqjs <- loadJQuery
--   bjs <- externalScript "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"
--   bcss <- externalCSS "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
--   return [jqjs,bjs,bcss]

loadBootstrap :: IO [DOMElement]
loadBootstrap = do
  jqjs <- loadJQuery
  bjs <- externalScript "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"
  bcss <- externalCSS "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
  return $ concat [jqjs,bjs,bcss]
