{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module EmbedGHCJS (ghcjsWebModule, GHCJSBindings) where

import WebModule.Markable
import WebModule.ServeEmbedded (serveDynamic, serveEmbedded, verifyEmbeddedFP)
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.ModuleScopeURL

import Happstack.Server
import Control.Monad.Trans as Trans (liftIO)
import Control.Monad as Monad (msum)
import System.FilePath
import System.Log.Logger (debugM, logM, Priority(ALERT, DEBUG, INFO), rootLoggerName, setHandlers, setLevel, updateGlobalLogger)

import Data.FileEmbed (embedDir)
import Data.Map as Map (Map, fromList)
import Data.ByteString as B (ByteString)


-- There's really nothing to run, since this will be main.
data GHCJSBindings = GHCJSBindings

baseurl:: ModuleScopeURL
baseurl = $(moduleScopeURL "")

basepath :: FilePath
basepath = moduleScopeURLtoFilePath baseurl

(+++) :: ModuleScopeURL -> FilePath -> ModuleScopeURL
(+++) = moduleScopeAppend

-- Hard-coded path because template haskell staging, didn't want it in another file.
ghcjsFileMap :: Map FilePath B.ByteString
-- ghcjsFileMap = Map.fromList $(embedDir "/usr/bin/happstack-ghcjs-client.jsexe")
ghcjsFileMap = Map.fromList $(embedDir "../happstack-ghcjs-client/dist/build/happstack-ghcjs-client/happstack-ghcjs-client.jsexe")

ghcjsWebModule :: Monad m => WebSiteM m GHCJSBindings
ghcjsWebModule = wimport ws GHCJSBindings
  where ws = mzeroWebSite { serverpart = ghcjsSP
                          , headers = [WMH_JavaScript (baseurl +++ jsFilePath)]
                          , bodies = []
                          , baseURL = [baseurl]
                          }

#ifdef DYNAMICLOADING
ghcjsSP :: ServerPartT IO Response
ghcjsSP = dir basepath $ uriRest (serveDynamic "ghcjs" ghcjsFileMap)
#else
ghcjsSP :: ServerPartT IO Response
ghcjsSP = dir basepath $ uriRest (serveEmbedded "ghcjs" ghcjsFileMap)
#endif

jsFilePath :: FilePath
[jsFilePath] =  map v fps
  where v = verifyEmbeddedFP "GHCJSWebModule:ghcjsFileMap" ghcjsFileMap
        fps = ["all.js"]
        