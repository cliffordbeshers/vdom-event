module Main where

import Control.Monad as Monad (msum)
import Control.Monad.Trans as Trans (liftIO)
import Data.Time.Format (FormatTime(..))
import Happstack.Server (asContentType, Browsing(EnableBrowsing), Conf(logAccess, port), dir, dirs, LogAccess, nullConf, Response, serveDirectory, serveFile, ServerPartT, simpleHTTP)
import Happstack.Server.Internal.LogFormat (formatRequestCombined)
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FilePath ((<.>), (</>))
import System.IO (stdout)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (debugM, logM, Priority(ALERT, DEBUG, INFO), rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
--import StaticResources
import EmbedGHCJS

-- Currently, the routing is messed up.

data LogMode
    = Production
    | Development
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

logMAccess' :: FormatTime t => LogAccess t
logMAccess' host user time requestLine responseCode size referer userAgent =
    logM "SimpleServer" DEBUG $ formatRequestCombined host user time requestLine responseCode size referer "" -- userAgent

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
       , serveDirectory EnableBrowsing [] "/home/beshers/alldarcs/src.seereason.com"
       ]

ghcjsHandler :: LogMode -> GHCJSPackageName -> ServerPartT IO Response
ghcjsHandler mode package =
  msum $ [ foo ] ++ map servejs ["/lib.js", "/rts.js", "/lib1.js", "/out.js" ]
  where servejs fp = do
          liftIO $ logM "Server.hs/ghcjsHandler" ALERT $ "Serving now " ++ (basepath package </> fp)
          dirs fp $ serveFile (asContentType "application/javascript") $ (basepath package) </> fp
        foo = dirs "/index.html" $ do liftIO $ putStrLn "Server.hs/ghcjsHandler" >> putStrLn ("Serving / with "  ++ (basepath package </> "index.html"))
                                      serveFile (asContentType "text/html") $ basepath package </> "index.html"
        basepath p = 
          case mode of
            Production -> "/usr/bin" </> (p <.> "jsexe")
            Development -> "/home/beshers/alldarcs/src.seereason.com/happstack-ghcjs-client/dist/build/happstack-ghcjs-client" </> (p <.> "jsexe")

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
