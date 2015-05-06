{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, ConstraintKinds #-}
module MicroDOM where

import GHCJS.Foreign
import GHCJS.Types

data DOMWindow = DOMWindow
data DOMDocument = DOMDocument
data DOMElement = DOMElement

foreign import javascript unsafe "$r = window"
  currentWindow :: IO (JSRef DOMWindow)
foreign import javascript unsafe "$r = document"
  currentDocument :: IO (JSRef DOMDocument)

foreign import javascript unsafe "$r = $1.createElement($2)"
  documentCreateElement :: JSRef DOMDocument -> JSString -> IO (JSRef DOMElement)

foreign import javascript unsafe "$r = document.createElement($1)"
  createElement' :: JSString -> IO (JSRef DOMElement)

createElement :: ToJSString a => a -> IO (JSRef DOMElement)
createElement s = createElement' (toJSString s)

foreign import javascript unsafe "$r = $1.createTextNode($2)"
  documentCreateTextNode :: JSRef DOMDocument -> JSString -> IO (JSRef DOMElement)

foreign import javascript unsafe "$r = document.createTextNode($1)"
  createTextNode' :: JSString -> IO (JSRef DOMElement)

createTextNode :: ToJSString a => a -> IO (JSRef DOMElement)
createTextNode n = createTextNode' (toJSString n)

foreign import javascript unsafe "$r = $1.setAttribute($2,$3)"
  setAttribute' :: JSRef DOMElement -> JSString -> JSString -> IO JSString

setAttribute :: ToJSString a => JSRef DOMElement -> a -> a -> IO JSString
setAttribute e k v = setAttribute' e (toJSString k) (toJSString v)

foreign import javascript unsafe "$r = $1.getAttribute($2)"
  getAttribute' :: JSRef DOMElement -> JSString -> IO JSString

getAttribute :: ToJSString a => JSRef DOMElement -> a -> IO JSString
getAttribute e k = getAttribute' e (toJSString k)

foreign import javascript unsafe "$r = document.body"
  documentBody :: IO (JSRef DOMElement)

foreign import javascript unsafe "$1.appendChild($2)"
  appendChild :: JSRef DOMElement -> JSRef DOMElement -> IO ()



