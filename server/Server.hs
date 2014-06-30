module Main where

import Control.Monad as Monad (msum)
import Data.Time.Format (FormatTime(..))

import Happstack.Server
import System.IO (hPutStrLn, stderr, stdout)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger -- (Priority(DEBUG), debugM, logM, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import Happstack.Server.Internal.LogFormat (formatRequestCombined)
--import StaticResources

logMAccess' :: FormatTime t => LogAccess t
logMAccess' host user time requestLine responseCode size referer userAgent =
    logM "SimpleServer" DEBUG $ formatRequestCombined host user time requestLine responseCode size referer "" -- userAgent


main :: IO ()
main = do
  -- stdoutLog <- streamHandler stdout DEBUG
  updateGlobalLogger "SimpleServer" (setLevel DEBUG)
  debugM "SimpleServer" "Global logger started at level DEBUG."
  let p = 8010
  debugM "SimpleServer" $ "Starting server on port " ++ show p
  simpleHTTP (nullConf { port = p, logAccess = Just logMAccess' }) $ handlers
  
handlers :: ServerPartT IO Response
handlers = 
  msum $ -- staticResourceParts "/static" ++
       [ dir "/manifest.appcache" $ serveFile (asContentType "text/cache-manifest") "manifest.appcache"
       , dir "/favicon.ico" $ serveFile (asContentType "image/x-icon") "favicon.ico"
       , dir "/" $ serveFile (asContentType "text/html") "dist/build/happstack-ghcjs-server/happstack-ghcjs-server.jsexe/index.html"
       , serveDirectory EnableBrowsing [] "."
       ]
