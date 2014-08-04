{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

-- import GHCJS.Concurrent
-- import GHCJS.Types
-- import GHCJS.Foreign
-- import GHCJS.Marshal
-- import GHCJS.DOM

import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML)
import GHC.Generics
import ZipTree
import AdminConsole (content)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import JavaScript.JQuery
import Data.Text.Encoding (decodeUtf8)
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Data.Aeson as A
import Data.Text as T (Text, unpack, pack)
import Data.Text.Encoding as TE (decodeUtf8)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Common
import           Data.Default
-- import Data.Text.Lazy as Text (Text, unpack, pack)

default(Text)



main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    putStrLn $ unpack $ tj $ MarshalMe 1 "one"
    putStrLn $ unpack $ tj $ MarshalMe 2 "two"
    ajaxJSON ajaxurl $ MarshalMe 1 "one"
    putStrLn $ unpack $ tj $ MarshalMe 2 "two"
    return ()
--    htmlElementSetInnerHTML body $ unpack $ renderHtml content

ajaxurl :: T.Text 
ajaxurl = "/ajax"


ajaxJSON :: ToJSON a => T.Text -> a -> IO AjaxResult
ajaxJSON url a = ajax url [("MarshalMe" :: Text, tj a)] def

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

