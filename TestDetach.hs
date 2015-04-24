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

foreign import javascript unsafe "$1.off()"
  jq_off' :: JQuery -> IO ()

convertHandlerSettings :: HandlerSettings -> (Bool, Bool, Bool, JSString, JSRef ())
convertHandlerSettings (HandlerSettings pd sp sip _ ds hd) =
  (pd, sp, sip, maybe jsNull toJSString ds, fromMaybe jsNull hd)

onn :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO (IO ())
onn a et hs jq = do
  cb <- if hsSynchronous hs
          then F.syncCallback1 F.AlwaysRetain True a
          else F.asyncCallback1 F.AlwaysRetain a
  jq_on cb et' ds hd sp sip pd jq
  return (jq_off' jq >> F.release cb >> print "jq_off")
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

