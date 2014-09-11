{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad as Monad (msum)
import Control.Monad.Trans as Trans (liftIO)
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
import LiveDevel
import LogType
import Common
-- Currently, the routing is messed up.

application :: GHCJSPackageName
application = "ghcjs-dom-hello"

main :: IO ()
main = do
  -- stdoutLog <- streamHandler stdout DEBUG
  updateGlobalLogger application (setLevel DEBUG)
  debugM application "Global logger started at level DEBUG."
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
       , dir "/favicon.ico" $ serveFile (asContentType "image/x-icon") "favicon.ico"
       , ghcjsHandler mode package
       , ajaxHandler
--       , serveDirectory EnableBrowsing [] "/home/beshers/alldarcs/src.seereason.com"
       ]

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
