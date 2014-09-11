{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad as Monad (msum)
import Control.Monad.Trans as Trans (liftIO)
import Data.Time.Format (FormatTime(..))
import Happstack.Server
import Happstack.Server.Internal.LogFormat (formatRequestCombined)
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FilePath ((<.>), (</>))
import System.IO (stdout)
import Data.Aeson as Aeson (decode)
import Manifest
import Template
import Data.Text as Text
import Data.FileEmbed
import qualified Data.ByteString as B

import qualified Text.Blaze.Html5 as H (body, div, docTypeHtml, head, link, meta, title, toMarkup)
import qualified Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, manifest, rel, type_)
import Text.Blaze.Html.Renderer.Utf8  (renderHtml)

debugM x y = putStr x >> putStr ":" >> putStrLn y

main :: IO ()
main = do
  let p = 8010
  debugM "SimpleServer" $ "Starting server on port " ++ show p
  simpleHTTP (nullConf { port = p }) $ handlers
  


handlers :: ServerPartT IO Response
handlers = msum [ rootHandler
                , manifestHandler [ WSE (Text.pack "index.html") (toStrict1 $ renderHtml application)
                                  , WSE (Text.pack "favicon.ico") favicon
                                  ]
                , faviconHandler favicon
                , notFound (toResponse ("plain text" ++ ":" ++ "notFound"))
                ]


application = htmlTemplate (Text.pack "Test Happstack") [] [H.div $ H.toMarkup "Hello, world! Foo!"]

rootHandler :: ServerPartT IO Response
rootHandler = msum [ nullDir >> ok (toResponse application)
                   , dirs "index.html" $ ok (toResponse application)
                   ]

manifestHandler :: WS -> ServerPartT IO Response
manifestHandler ws = dirs "manifest.appcache" $ ok $ setHeaders $ toResponse (genManifest ws)
  where setHeaders = setMimeType . setCache . setExpires
        setMimeType = setHeader "Content-Type" manifestMimeType
        setCache = setHeader "Cache-Control" "no-cache"
        setExpires = setHeader "Expires" "Thu, 01 Dec 1994 16:00:00 GMT" -- In the past, so instantly expires.
        manifestMimeType = "text/cache-manifest"

favicon :: B.ByteString
favicon = $(embedFile "server/favicon.ico")

faviconHandler :: B.ByteString -> ServerPartT IO Response
faviconHandler favicon = dirs "/favicon.ico" $ ok $ setMimeType $ toResponse favicon
  where setMimeType = setHeader "Content-Type" manifestMimeType
        manifestMimeType = "text/cache-manifest"

