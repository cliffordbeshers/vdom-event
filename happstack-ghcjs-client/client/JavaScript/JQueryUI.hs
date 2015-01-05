{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, EmptyDataDecls #-}

module JavaScript.JQueryUI where

import JavaScript.JQuery.Internal
import GHCJS.Types
import GHCJS.DOM.Types (Element(..))
import GHCJS.Foreign


#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$2.sortable($1)" jq_sortable  :: JSString -> JQuery -> IO JQuery

sortable :: JQuery -> IO JQuery
sortable = jq_sortable jsNull

#else
jq_sortable :: JSString -> JQuery -> IO JQuery
jq_sortable = error "JavaScript.JQueryUI.jq_sortable: only available in JavaScript"
sortable :: JQuery -> IO JQuery
sortable = error "JavaScript.JQueryUI.sortable: only available in JavaScript"

#endif
