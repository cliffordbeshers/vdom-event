{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import JavaScript.JQuery
import JavaScript.JQuery.Internal
import GHCJS.Foreign as F
import GHCJS.Types
import GHCJS.DOM
--import GHCJS.DOM.Event
--import GHCJS.DOM.EventM
import GHCJS.DOM.Types (GObjectClass)
import GHCJS.DOM.EventTargetClosures

default (Text)

--f ::  (GObjectClass self, ToJSString eventName, IsEvent event) =>
--      self -> eventName -> Bool -> (self -> event -> IO ()) -> IO (IO ())
--f = eventTargetAddEventListener
--  detachDo <- eventTargetAddEventListener buttonDo

def' :: HandlerSettings
def' = def { hsSynchronous = False }


click' a = onn a "click"

foreign import javascript unsafe "$4.off($2,$3,$1)"
  jq_off' :: JSRef c                -- ^ callback
         -> JSString               -- ^ event type
         -> JSString               -- ^ descendant selector
         -> JQuery
         -> IO (JSFun ())


foreign import javascript unsafe "((function () { lr=h$jquery_makeListener($1, $5, $6, $7); $8.on($2, $3, $4,lr); return lr; })())"
  jq_on' :: JSFun a                -- ^ callback
        -> JSString               -- ^ event type
        -> JSString               -- ^ descendant selector
        -> JSRef b                -- ^ data
        -> Bool                   -- ^ stopPropagation
        -> Bool                   -- ^ stopImmediatePropagation
        -> Bool                   -- ^ preventDefault
        -> JQuery
        -> IO (JSRef c)


convertHandlerSettings :: HandlerSettings -> (Bool, Bool, Bool, JSString, JSRef ())
convertHandlerSettings (HandlerSettings pd sp sip _ ds hd) =
  (pd, sp, sip, maybe jsNull toJSString ds, fromMaybe jsNull hd)

-- foreign import javascript unsafe "h$makeCallback($1, h$runSync, [$2], $3)"
--   js_syncCallback :: JSRef a -> Bool -> Int -> IO (JSFun (IO b))
-- foreign import javascript unsafe "h$makeCallback($1, h$run, [], $2)"
--   js_asyncCallback :: JSRef a -> Int -> IO (JSFun (IO b))

-- foreign import javascript unsafe "h$makeCallbackApply($1, $3, h$runSync, [$2], $4)"
--   js_syncCallbackApply :: JSRef a -> Bool -> Int -> Int -> IO (JSRef b)
-- foreign import javascript unsafe "h$makeCallbackApply($1, $2, h$run, [], $3)"
--   js_asyncCallbackApply :: JSRef a -> Int -> Int -> IO (JSRef b)

foreign import javascript unsafe "h$jquery_makeListener($1, $2, $3, $4);"
  jq_makeListener :: JSFun a -> Bool -> Bool -> Bool -> IO (JSRef b)

onn :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO (IO ())
onn a et hs jq = do
  cb <- if hsSynchronous hs
          then F.syncCallback1 F.AlwaysRetain True a
          else F.asyncCallback1 F.AlwaysRetain a
  cb' <- jq_on' cb et' ds hd sp sip pd jq
  return (jq_off' cb' et' ds jq >> F.release cb >> print "jq_off")
    where
      et'                   = toJSString et
      (pd, sp, sip, ds, hd) = convertHandlerSettings hs


main = do
  body <- select "body"
  buttonDo <- select "<button>Should do something visible</button>"
  buttonDont <- select "<button>Should disable other button</button>"
  appendJQuery buttonDo body
  appendJQuery buttonDont body
  detachDo <- click' (\e -> void (select "<div>Ugh, it did something.</div>" >>= flip appendJQuery body)) def buttonDo
  click (\e -> detachDo >> print "detachDo" ) def buttonDont
  return ()

