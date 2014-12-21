{-# LANGUAGE CPP, QuasiQuotes, StandaloneDeriving, EmptyDataDecls #-}
{-# OPTIONS -fno-warn-orphans #-}

module AppraisalScribe.JMacro ( avrPost
                              , avrGet
                              , buttonGetURL
                              , download
                              , upload
                              , post
                              , postS
                              , postSS
                              , unpost
                              , unpostS
                              , thisValue
                              , thunk
                              , oldValue
                              , nullJS
                              , JSS(..)
                              , JSE(..)
                              ) where

import Control.Arrow (second)
import Data.Char (toLower)
import qualified Data.Map as M
import Data.Monoid
import Language.Javascript.JMacro
import Data.Text as T
import Lucid
import Text.JSON
import Network.URI
-- import MyURL
import AppraisalScribe.ArrowState (ArrowState(..))
import AppraisalScribe.JSGetSet
import AppraisalScribe.JQuerySelector

-- JSON instance (preserved from jmacro-0.5.8, before the aeson migration.)
instance ToJExpr JSValue where
    toJExpr JSNull             = ValExpr $ JVar $ StrI "null"
    toJExpr (JSBool b)         = ValExpr $ JVar $ StrI $ Prelude.map Data.Char.toLower (show b)
    toJExpr (JSRational _ rat) = ValExpr $ JDouble $ realToFrac rat
    toJExpr (JSString s)       = ValExpr $ JStr $ fromJSString s
    toJExpr (JSArray vs)       = ValExpr $ JList $ Prelude.map toJExpr vs
    toJExpr (JSObject obj)     = ValExpr $ JHash $ M.fromList $ Prelude.map (second toJExpr) $ fromJSObject obj

data JSS a = JSS JStat
data JSE a = JSE JExpr

-- deriving instance Show Response

-- instance ToValue JExpr where
--     toValue = toValue . show . renderJs

-- instance ToValue JStat where
--     toValue = toValue . show . renderJs

-- instance ToMarkup JStat where
--     toMarkup = preEscapedToMarkup . show . renderJs

instance ToJExpr URI where
  toJExpr = toJExpr . show

-- instance ToJExpr T.Text where
--   toJExpr = toJExpr . T.unpack

nullJS :: JStat
nullJS = mempty

buttonGetURL :: String -> JExpr
buttonGetURL url = [jmacroE|function(){$.get(`(url)`);}|]


thunk :: JStat -> JExpr
thunk s = [jmacroE|function(){`(s)`;}|]

-- this function attaches directly to the onfocus event of a widget

avrGet :: JStat
avrGet=[jmacro|function !avrGet(url, arr, objset) {
                   arrowSetState(`(arr)`, `(Sending)`);
                   console.log(`(arr)` + `(Sending)`);
                   var jqxhr = $.ajax( {type: "GET", url: url, success: objset, dataType: "json" } );
                   // alert(url);
                 };
         |]

-- post path = HA.onclick $ renderJs [jmacroE|avrPost(path
avrPost :: JStat
avrPost=[jmacro|function !avrPost(arrow,url,value,oldvalue) {
                   var vv=JSON.stringify(value);
                   var ov=JSON.stringify(oldvalue);
                   var callback = avrDebug;
                   var jqxhr = $.post(url, {value: vv, oldvalue: ov}, (function(x) {avrDebug(x); callback(x);}), "json");
                   console.log('avrPost'+ url);
                 };
                fun avrDebug args { console.log("post received: " + args); };
 |]

#if UNUSED
avrPostS :: JStat
avrPostS=[jmacro|function !avrPost(arrow,url,value,oldvalue, failure, complete, success) {
                   var vv=JSON.stringify(value);
                   var ov=JSON.stringify(oldvalue);
                   var callback = avrDebug;
                   var jqxhr = $.post(url, {value: vv, oldvalue: ov}, (function(x) {avrDebug(x); callback(x);}), "json");
                   console.log('avrPost'+ url);
                 };
                fun avrDebug args { console.log("post received: " + args); };
 |]
#endif


{-
-- avrPost' :: JSE (
avrPost' = [jmacro|fun avrPost url value oldvalue {
                      var jqxhr = $.post(url, {value: value, oldvalue : oldvalue}, avrPostCallback, "json");
                      // alert(url);
                    };
            fun avrPostCallback args { avrDebug(args); };
            fun avrDebug args { $("#post_info").text("post received: " + args); };
         |]
-}


download :: (JSGetSet a) => a -> URI -> JExpr
download _ uri = jsUndefined (show uri) -- get a avrGet

upload :: JSGetSet a => a -> URI -> JExpr
upload _ uri = jsUndefined (show uri)
-- avrGet(objset, url);

-- div[class~="avrPost"]

post :: (ToJExpr a2, ToJExpr a1) => URI -> a1 -> a2 -> JExpr
post uri value oldvalue=[jmacroE|avrPost(`(show uri)`, `(value)`, `(oldvalue)`)|]

postS :: (ToJExpr url, ToJExpr a) => JSelector -> url -> a -> a -> JStat
postS arr uri value oldvalue =
  [jmacro|{ arrowSetState(`(arr)`,`(Sending)`);
           console.log("postS");
           avrPost(`(arr)`, `(uri)`, `(value)`, `(oldvalue)`);
           }
             |]

postSS :: (ToJExpr url, ToJExpr a) => JSelector -> JExpr -> url -> a -> a -> JStat
postSS arr postJS uri value oldvalue =
  [jmacro|{ arrowSetState(`(arr)`,`(Sending)`);
           console.log("postS");
           avrPostS(`(arr)`, `(postJS)`, `(uri)`, `(value)`, `(oldvalue)`);
           }
             |]

unpost :: String -> URI -> JExpr
unpost ident uri = [jmacroE|avrGet(`(show uri)`, (function (vv) { $(`(ident)`).val(vv); $("input").data('oldvalue',vv);}))|]
--unpost uri = [jmacroE|avrGet(`(show uri)`, (function (v) { $(this).val(v); }))|]

unpostS :: ToJExpr url => JSelector -> String -> url -> JExpr
unpostS arr textformId uri = [jmacroE|avrGet(`(uri)`, `(arr)`, (function (vv) { var tf = $(`(textformId)`); tf.val(vv); tf.data('oldvalue',vv);}))|]
--unpost uri = [jmacroE|avrGet(`(show uri)`, (function (v) { $(this).val(v); }))|]

data Object --  = Object

class JSLens a where
    lensSet :: (ToJExpr a) => JSE (Object -> a -> ())
    lensGet :: (ToJExpr a) => JSE (Object -> (a,a))

-- Anything that is a standard form can use jQuery .val() function to get/set.
-- Using jQuery.data for oldvalue will work for any object.
instance JSLens T.Text where
    lensSet = formSet
    lensGet = formGet

-- These have no monkey business about checking old versus new.  Set
-- applies the new value to both new and old. Get returns both the new and old.
formSet :: (ToJExpr a) => JSE (Object -> a -> ())
formSet = JSE [jmacroE|function (obj,vv) { obj.val(vv); jQuery.data(obj,'oldvalue',vv); } |]

formGet :: (ToJExpr a) => JSE (Object -> (a,a))
formGet = JSE [jmacroE|function (obj,vv) { obj.val(vv); jQuery.data(obj,'oldvalue',vv); } |]


thisValue :: JExpr
thisValue = [jmacroE|$(this).val()|]
oldValue :: JExpr
oldValue = [jmacroE|$(this).data('oldvalue')|]

-- oldValue = [jmacroE|$(this).data('oldValue')|]

{-
testRPCCall :: String -> JExpr -> JExpr -> JExpr
testRPC :: WebRPCDesc
(testRPC, testRPCCall) = mkWebRPC "test" $ \x y -> asIO $ return (x + (y::Int))
-}

{-
arg1 = [jmacroE|1|]
arg2 = [jmacroE|2|]
-}

{-
w :: String -> JExpr -> JExpr -> JExpr
v :: (String, Request -> IO Response)
(v, w) = mkWebRPC "my" $ \x y -> asIO $ return ((x::Int) + y::Int)
-}



-- setViewport :: JSE (Selector Image) -> JSE Int -> JSE Int -> JSE Int -> JSE Int -> JSS (IO ())
{-
setViewport :: String -> JStat
setViewport view = [jmacro|function setViewport (c) {
                              var sel = `(view)`;
                              var w;
                              var h;
                              w = $
                              $(sel).css({ width: Math.round(rx * 800) + 'px'
                                         , height: Math.round(ry * 600) + 'px'
                                         , marginLeft: '-' + Math.round(rx * cx) + 'px'
                                         , marginTop: '-' + Math.round(ry * cy) + 'px'
                                         });
                            }
                    |]
-}



{-

-- foo :: String
foo = sequence_ $ foldl (\x -> print (renderJs x) >> putStrLn "--")
      [ [jmacro|var id = \x -> x; var id = \x -> x;|]
      , [jmacro|var id = \x -> x; var id = \x -> x;|]
      ]
fooE = mapM_ (\x -> print (renderJs x) >> putStrLn "--")
       [ [jmacroE|foo(x,y)|]
       , [jmacroE|foo x y|]
       ]
-}
