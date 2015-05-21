{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, ConstraintKinds #-}
module MicroDOM where

import GHCJS.Foreign
import GHCJS.Types

data DOMWindow_ = DOMWindow_
-- type DOMWindow = JSRef DOMWindow_
type DOMWindow = JSRef ()

data DOMDocument_ = DOMDocument_
-- type DOMDocument = JSRef DOMDocument_
type DOMDocument = JSRef ()

data DOMElement_ = DOMElement_
-- type DOMElement = JSRef DOMElement_
type DOMElement = JSRef ()

data DOMEvent_ = DOMEvent_
-- type DOMEvent = JSRef DOMEvent_
type DOMEvent = JSRef ()

foreign import javascript unsafe "$r = window"
  currentWindow :: IO (DOMWindow)
foreign import javascript unsafe "$r = document"
  currentDocument :: IO (DOMDocument)

foreign import javascript unsafe "$r = $1.createElement($2)"
  documentCreateElement :: DOMDocument -> JSString -> IO (DOMElement)

foreign import javascript unsafe "$r = document.createElement($1)"
  createElement' :: JSString -> IO (DOMElement)

createElement :: ToJSString a => a -> IO (DOMElement)
createElement s = createElement' (toJSString s)

foreign import javascript unsafe "$r = $1.createTextNode($2)"
  documentCreateTextNode :: DOMDocument -> JSString -> IO (DOMElement)

foreign import javascript unsafe "$r = document.createTextNode($1)"
  createTextNode' :: JSString -> IO (DOMElement)

createTextNode :: ToJSString a => a -> IO (DOMElement)
createTextNode n = createTextNode' (toJSString n)

foreign import javascript unsafe "$r = $1.setAttribute($2,$3)"
  setAttribute' :: DOMElement -> JSString -> JSString -> IO JSString

setAttribute :: ToJSString a => DOMElement -> a -> a -> IO JSString
setAttribute e k v = setAttribute' e (toJSString k) (toJSString v)

foreign import javascript unsafe "$r = $1.getAttribute($2)"
  getAttribute' :: DOMElement -> JSString -> IO JSString

getAttribute :: ToJSString a => DOMElement -> a -> IO JSString
getAttribute e k = getAttribute' e (toJSString k)

foreign import javascript unsafe "$r = document.body"
  documentBody :: IO (DOMElement)

foreign import javascript unsafe "$1.appendChild($2)"
  appendChild :: DOMElement -> DOMElement -> IO ()

foreign import javascript unsafe "$r = $1.firstChild"
  firstChild :: DOMElement -> IO DOMElement

foreign import javascript unsafe "$r = $1.lastChild"
  lastChild :: DOMElement -> IO DOMElement

foreign import javascript unsafe "$1.removeChild($2)"
  removeChild :: DOMElement -> DOMElement -> IO ()

foreign import javascript unsafe
        "$1.addEventListener($2, $3, $4)"
        addEventListener ::
        DOMElement -> JSString -> JSRef a -> Bool -> IO ()

getFirstChild :: DOMElement -> IO (Maybe DOMElement)
getFirstChild e = do
  c <- firstChild e
  if isNull c then return Nothing else return (Just c)

getLastChild :: DOMElement -> IO (Maybe DOMElement)
getLastChild e = do
  c <- lastChild e
  if isNull c then return Nothing else return (Just c)



foreign import javascript unsafe
        "$1.removeEventListener($2, $3, $4)"
        removeEventListener ::
        DOMElement -> JSString -> JSRef a -> Bool -> IO Bool

eventTargetAddEventListener :: ToJSString eventName =>
                           DOMElement -> eventName -> Bool -> (DOMElement -> DOMEvent -> IO ()) -> IO (IO ())
eventTargetAddEventListener element eventName bubble user = do
    callback <- syncCallback1 AlwaysRetain True $ \e -> user element e
    addEventListener
        element
        (toJSString eventName)
        callback
        bubble
    return $ do
        removeEventListener
            element
            (toJSString eventName)
            callback
            bubble
        release callback

maybeJSNull :: JSRef a -> Maybe (JSRef a)
maybeJSNull r | isNull r = Nothing
maybeJSNull r = Just r
