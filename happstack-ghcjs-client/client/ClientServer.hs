{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- TODO WebModule.AJAXModule.GetAJAXConnectedToSortable
-- TODO "happstack-ghcjs-server" Main.ReplaceServerHSWithClientServerFromClient
-- TODO "happstack-ghcjs-server" WebModule.WebModule.RepresentPageStateMonad
-- TODO "happstack-ghcjs-client" Tasks.TaskManagerInterface


-- import GHCJS.Concurrent
-- import GHCJS.Types
-- import GHCJS.Foreign
-- import GHCJS.Marshal
-- import GHCJS.DOM

import GHC.Generics
-- import ZipTree
import AdminConsole (content)
-- import Text.Blaze.Html.Renderer.Text (renderHtml)
#if CLIENT
import JavaScript.JQuery
#else
import GHCJSStub.JQuery
#endif
import Data.Text.Encoding (decodeUtf8)
#if CLIENT
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.DOM (WebView, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML)
#else
import GHCJSStub.Types
import GHCJSStub.Foreign
import GHCJSStub.Marshal
import GHCJSStub.DOM (webViewGetDomDocument, runWebGUI)
import GHCJSStub.DOM.Document (documentGetBody)
import GHCJSStub.DOM.HTMLElement (htmlElementSetInnerHTML)
#endif

import Data.Aeson as A
import qualified Data.Text as T (Text, unpack, pack)
import qualified Data.Text.Lazy as LT (Text, unpack, pack, toStrict)
import Data.Text.Encoding as TE (decodeUtf8)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import Text.Blaze.Html5 as H (Markup, toMarkup, ul, li)

import WebModule.GName
import GHC.Generics
import Data.Typeable as Typeable (Proxy(..))


import Common
import           Data.Default
-- import Data.Text.Lazy as Text (Text, unpack, pack)
import WebModule.WebModule
import WebModule.WebModuleM 
import WebModule.SortableModule as Sortable
import LucidExample
import WidgetPalette.Index
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Writer (tell)

import WebModule.Favicon
import WebModule.Markable
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.JQueryWebModule
import WebModule.JQueryUIWebModule
import WebModule.BootstrapWebModule
import WebModule.GHCJSWebModule
import WebModule.ServeEmbedded (EmbeddedDirectory, embedDirectoryTH)
import WebModule.AJAXModule as AJAX

#if SERVER
import Happstack.Server
#endif

default(T.Text)
#if CLIENT
type RETURN = WebView -> IO ()
#else
type RETURN = ()
#endif

faviconWebSite :: WebSite
faviconWebSite = 
  WebSite {
#if CLIENT
 serverpart = undefined
#else
 serverpart = faviconHandler favicon
#endif
          , baseURL = [faviconURLMS]
          , headers = [WMH_Favicon faviconURLMS]
          , bodies = []
          , manifest = []
          }


main :: IO ()
#if CLIENT
main = do
  gui <- runClientWebSiteM clientServerWebSite
  runWebGUI gui
#else 
main = do
  let p = 8010
  print ("Serving on localhost",p)
  (_, ws) <- compileWebSiteM clientServerWebSite
  simpleHTTP (nullConf { port = p }) $ serverpart ws
#endif

#if CLIENT
runClientWebSiteM :: WebSiteM IO (WebView -> IO ()) -> IO (WebView -> IO ())
runClientWebSiteM mws = do
  (ws,_) <- runWebSiteM mws
  return $ \webView ->
   do
    Just doc <- webViewGetDomDocument webView
    body' <- documentGetBody doc
    -- TODO client/ClientServer.hs -- documentGetBody sometimes runs too early, add wait loop
    Just body <- documentGetBody doc
    ws webView
#endif

#ifdef SERVE_DYNAMIC
ghcjsFiles = Left ("../happstack-ghcjs-client/dist/build/happstack-ghcjs-client", "happstack-ghcjs-client.jsexe")
#else
ghcjsFiles = Right $(embedDirectoryTH "/usr/bin" "happstack-ghcjs-client.jsexe")
-- _ = True == verifyGHCJSFileMap ghcjsFiles
#endif

mm :: AJAXType MarshalMe
mm = AJAXType tname (BSL.toStrict . encode) decode
  where tname = gname (Proxy :: Proxy MarshalMe)

clientServerWebSite ::  WebSiteM IO (RETURN)
clientServerWebSite =  do
  tell faviconWebSite
  -- GHCJS is now rolling the .js files in.  Pain. Moving it up overrides.
  GHCJSBindings{..} <- ghcjsWebModule ghcjsFiles
  JQueryBindings{..} <- jQueryModule
  JQueryUIBindings{..} <- jQueryUIModule
  BootstrapBindings{..} <- bootstrapModule
  Sortable.SortableBindings{..} <- Sortable.sortableWebModule
  AJAX.AJAXBindings{..} <- AJAX.ajaxModuleGen mm
#if CLIENT
  return (\webView -> lucidExample >> return ())
#else
  return ()
#endif


sampleList :: [Markup]
sampleList = map H.toMarkup $ map (\n -> "Item " ++ show n) [1..4 :: Int] 

#if 0
#if CLIENT
ajaxJSON :: ToJSON a => T.Text -> a -> IO AjaxResult
ajaxJSON url a = ajax url [(T.pack messageKey, tj a)] def
#endif

tj :: ToJSON a => a -> T.Text
tj = TE.decodeUtf8 . toStrict1 . A.encode 

marshalledText :: T.Text
marshalledText = TE.decodeUtf8 marshalledByteString

marshalledByteString :: BS.ByteString
marshalledByteString = toStrict1 $ A.encode $  MarshalMe 1 "one"
#endif


