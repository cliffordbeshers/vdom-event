{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

-- import GHCJS.Concurrent
-- import GHCJS.Types
-- import GHCJS.Foreign
-- import GHCJS.Marshal
-- import GHCJS.DOM

import GHC.Generics
import ZipTree
import AdminConsole (content)
import Text.Blaze.Html.Renderer.Text (renderHtml)
#ifdef ghcjs_HOST_OS
import JavaScript.JQuery
#else
import GHCJSStub.JQuery
#endif
import Data.Text.Encoding (decodeUtf8)
#ifdef ghcjs_HOST_OS
import GHCJS.Types
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML)
#else
import GHCJSStub.Types
import GHCJSStub.Types
import GHCJSStub.Foreign
import GHCJSStub.Marshal
import GHCJSStub.DOM (webViewGetDomDocument, runWebGUI)
import GHCJSStub.DOM.Document (documentGetBody)
import GHCJSStub.DOM.HTMLElement (htmlElementSetInnerHTML)
#endif
import Data.Aeson as A
import qualified Data.Text as T (Text, unpack, pack)
import qualified Data.Text.Lazy as LT (Text, unpack, pack)
import Data.Text.Encoding as TE (decodeUtf8)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Common
import           Data.Default
-- import Data.Text.Lazy as Text (Text, unpack, pack)
import Sortable
default(T.Text)



main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    let message = MarshalMe 13 "thirteen"
    putStrLn $ T.unpack $ tj $ message
    ajaxJSON ajaxURLT $ message
    htmlElementSetInnerHTML body $ LT.unpack $ renderHtml content


ajaxJSON :: ToJSON a => T.Text -> a -> IO AjaxResult
ajaxJSON url a = ajax url [(T.pack messageKey, tj a)] def

tj :: ToJSON a => a -> T.Text
tj = TE.decodeUtf8 . toStrict1 . A.encode 

marshalledText :: T.Text
marshalledText = TE.decodeUtf8 marshalledByteString

marshalledByteString :: B.ByteString
marshalledByteString = toStrict1 $ A.encode $  MarshalMe 1 "one"

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks



--ttt :: IO Text
--ttt = (toJSRef_aeson $  MarshalMe 1 "one")


-- ajax' :: Text -> [(Text,Text)] -> AjaxSettings -> IO AjaxResult
-- ajax' = ajax

-- foop :: ToJSON a => a -> IO AjaxResult
-- foop a = do
--   let a' = decodeUtf8 $  encode a
--   ajax "/ajax" [("MarshalMe", a')] def

