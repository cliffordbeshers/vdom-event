{-# LANGUAGE CPP #-}

#ifdef ghcjs_HOST_OS
#define SERVER 0
#define CLIENT 1
#else
#define SERVER 1
#define CLIENT 0
#endif

#if CLIENT
import GHCJS.DOM (webViewGetDomDocument, runWebGUI)

import WebImport
import WebImport.JQuery
import WebImport.Bootstrap
#endif

#if SERVER
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

import LiveDevel

#endif


#if SERVER
data LogMode
    = Production
    | Development
    deriving (Read, Show, Eq, Ord, Enum, Bounded)
#endif

main = 
#if CLIENT
  runWebGUI $ \ webView -> do
    putStrLn "ghcjs_HOST_OS"
#else
  putStrLn "ghc_HOST_OS"
#endif

#if SERVER
logMAccess' :: FormatTime t => LogAccess t
logMAccess' host user time requestLine responseCode size referer userAgent =
    logM "SimpleServer" DEBUG $ formatRequestCombined host user time requestLine responseCode size referer "" -- userAgent

application :: GHCJSPackageName
application = "happstack-ghcjs"

serverMain :: IO ()
serverMain = do
  -- stdoutLog <- streamHandler stdout DEBUG
  updateGlobalLogger application (setLevel DEBUG)
  debugM application "Global logger started at level DEBUG."
  let p = 8010
  debugM "SimpleServer" $ "Starting server on port " ++ show p
  let logDir = "_local"
  setupLogger logDir Development
  logM "Main.hs" ALERT $ "Setting logging directory to be top directory (" ++ logDir ++ ") "
  simpleHTTP (nullConf { port = p, logAccess = Just logMAccess' }) $ handlers Development

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
#endif

--  do JQuery{..} <- wimport jQuery
     
   