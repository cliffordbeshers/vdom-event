{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, EmptyDataDecls #-}
module GHCJSStub.JQuery where

import Data.Default
import Data.Text
import Data.Typeable
#ifdef ghcjs_HOST_OS
import GHCJS.Types
import GHCJS.DOM.Types (Element(..))
import GHCJS.Foreign
#else
import GHCJSStub.Types
import GHCJSStub.DOM.Types (Element(..))
import GHCJSStub.Foreign
#endif

import Control.Concurrent.MVar

data JQuery_
data Event_

type JQuery = JSRef JQuery_
type Event = JSRef Event_

type EventType = Text
type Selector  = Text

data Method = GET | POST | PUT | DELETE deriving (Eq, Ord, Enum, Show)

data AjaxSettings = AjaxSettings { asContentType :: Text
                                 , asCache       :: Bool
                                 , asIfModified  :: Bool
                                 , asMethod      :: Method
                                 } deriving (Ord, Eq, Show, Typeable)

data AjaxResult = AjaxResult { arStatus :: Int
                             , arData   :: Maybe Text
                             } deriving (Ord, Eq, Show, Typeable)

data HandlerSettings = HandlerSettings { hsPreventDefault           :: Bool
                                       , hsStopPropagation          :: Bool
                                       , hsStopImmediatePropagation :: Bool
                                       , hsSynchronous              :: Bool
                                       , hsDescendantFilter         :: Maybe Selector
                                       , hsHandlerData              :: Maybe (JSRef ())
                                       }

instance Default HandlerSettings where
  def = HandlerSettings False False False True Nothing Nothing

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$2.addClass($1)"       jq_addClass          :: JSString             -> JQuery -> IO JQuery

#else
jq_addClass :: JSString -> JQuery -> IO JQuery
jq_addClass = error "jq_addClass: only available in JavaScript"
#endif

on :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO (IO ())
on a et hs jq = undefined

select :: Text -> IO JQuery
select = undefined
