{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad as Monad (msum)
import Control.Monad.Trans as Trans (liftIO)
import Data.Text as Text (Text, pack)
import Data.Time.Format (FormatTime(..))
import Happstack.Server
import Happstack.Server.Internal.LogFormat (formatRequestCombined)
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FilePath ((<.>), (</>))
import System.IO (stdout)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (debugM, logM, Priority(ALERT, DEBUG, INFO), rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
--import StaticResources
import Data.Aeson as Aeson (decode)
import EmbedGHCJS
import EmbeddedPath
import BootstrapModule 
import PDFObjectModule 
import Manifest
import ManifestURL
import Favicon
import WebModule
import LiveDevel
import LogType
import Common
import Template
-- Currently, the routing is messed up.
import qualified Text.Blaze.Html5 as H (Markup, body, div, docTypeHtml, head, link, meta, title, toMarkup)
import qualified Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, manifest, rel, type_)
import Text.Blaze.Html.Renderer.Utf8  (renderHtml)
import qualified Data.ByteString as B
import Data.FileEmbed

applicationName :: GHCJSPackageName
applicationName = "TestHappstack"

application :: H.Markup
application = htmlTemplate (Text.pack "Test Happstack") [] [H.div $ H.toMarkup "Hello, world! FooFooFooFoo!"]

main :: IO ()
main = do
  -- stdoutLog <- streamHandler stdout DEBUG
  updateGlobalLogger applicationName (setLevel DEBUG)
  debugM applicationName "Global logger started at level DEBUG."
  let p = 8010
  debugM "SimpleServer" $ "Starting server on port " ++ show p
  let logDir = "_local"
  setupLogger logDir Development
  logM "Main.hs" ALERT $ "Setting logging directory to be top directory (" ++ logDir ++ ") "
  simpleHTTP (nullConf { port = p, logAccess = Just logMAccess' }) $ handlers Development
  
handlers :: LogMode -> ServerPartT IO Response
handlers mode = do
  let package = "happstack-ghcjs-client"
  -- liftIO $ logM "Server.hs/handlers" ALERT $ "Serving " ++ package
  msum $ -- staticResourceParts "/static" ++
       [ dir "/manifest.appcache" $ serveFile (asContentType "text/cache-manifest") "manifest.appcache"
       , manifestHandler [ wse "text/html" (Text.pack "index.html") (toStrict1 $ renderHtml application)
                         , wse (Text.pack "favicon.ico") favicon
                         ]
       , rootHandler
       , faviconHandler favicon
       , ghcjsHandler mode package
       , ajaxHandler
       ]

rootHandler :: ServerPartT IO Response
rootHandler = msum [ nullDir >> ok (toResponse application)
                   , dirs "index.html" $ ok (toResponse application)
                   ]



-- manifestHandler :: WS -> ServerPartT IO Response
manifestHandler ws = dirs "manifest.appcache" $ ok $ setHeaders $ toResponse (genManifest ws)
  where setHeaders = setMimeType . setCache . setExpires
        setMimeType = setHeader "Content-Type" manifestMimeType
        setCache = setHeader "Cache-Control" "no-cache"
        setExpires = setHeader "Expires" "Thu, 01 Dec 1994 16:00:00 GMT" -- In the past, so instantly expires.
        manifestMimeType = "text/cache-manifest"


ajaxHandler :: ServerPartT IO Response
ajaxHandler = dirs ajaxURL $ h
  where h = do
          rq <- askRq 
          liftIO $ print rq
          let rqpath = foldr (</>) "" $ rqPaths rq
          liftIO $ print rqpath
          decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
          msgValue <- lookBS $ messageKey
          let msg :: Maybe MarshalMe = decode msgValue
          liftIO $ do
            putStrLn "\n\n\nThe message is:"
            print msg
            putStrLn "\n\n\n"
          ok $ toResponse "life model decoy"


-- zingHandler :: LogMode -> GHCJSPackageName -> ServerPartT IO Response
-- zingHandler mode package =
--   dir "zing" $ application

setupLogger :: FilePath -> LogMode -> IO ()
setupLogger logDir m = do
    createDirectoryIfMissing True logDir
    appLog    <- fileHandler (logDir </> "app.log")   DEBUG
    accessLog <- fileHandler (logDir </> "access.log") INFO
    stdoutLog <- streamHandler stdout DEBUG
    case m of
      Development -> do
          -- Root Log
          updateGlobalLogger rootLoggerName
            (setLevel DEBUG . setHandlers [appLog, stdoutLog])
          -- Access Log
          updateGlobalLogger "Happstack.Server.AccessLog.Combined"
            (setLevel DEBUG . setHandlers [accessLog])

      Production -> do
          -- Root Log
          updateGlobalLogger rootLoggerName
            (setLevel INFO . setHandlers [appLog])
          -- Access Log
          updateGlobalLogger "Happstack.Server.AccessLog.Combined"
            (setLevel INFO . setHandlers [accessLog])


