{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, EmptyDataDecls #-}

module JavaScript.JQueryExtra where

import Prelude hiding (show)

import JavaScript.JQuery.Internal
import GHCJS.Types
import GHCJS.DOM.Types (Element(..))
import GHCJS.Foreign


#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$2.hide($1)"       jq_hide          :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.show($1)"       jq_show          :: JSString             -> JQuery -> IO JQuery

hide :: JQuery -> IO JQuery
hide = jq_hide jsNull

show :: JQuery -> IO JQuery
show = jq_show jsNull


#else
jq_hide          :: JSString             -> JQuery -> IO JQuery
jq_hide                          = error "jq_hide: only available in JavaScript"
jq_show          :: JSString             -> JQuery -> IO JQuery
jq_show                          = error "jq_show: only available in JavaScript"

#endif
