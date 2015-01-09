{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module WebModule.GHCJSWebModule (ghcjsWebModule, GHCJSBindings(..)) where

import WebModule.Markable
import WebModule.ServeEmbedded
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.ModuleScopeURL

import Happstack.Server

import Data.FileEmbed (embedDir)
import Data.Map as Map (Map, fromList)
import Data.ByteString as B (ByteString)


-- There's really nothing to run, since this will be main.
data GHCJSBindings = GHCJSBindings { start :: Int }

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

ghcjsBindings :: GHCJSBindings
ghcjsBindings = GHCJSBindings { start = 1 }

ghcjsWebModule :: Monad m => WebSiteM m GHCJSBindings
ghcjsWebModule = wimport ws ghcjsBindings
  where ws = mzeroWebSite { serverpart = ghcjsSP
                          , headers = [WMH_JavaScript (baseurl +++ jsFilePath)]
                          , bodies = [WMB_Initialization "console.log('GHCJSWebModule initialization');"]
                          , baseURL = [baseurl]
                          }

ghcjsSP :: ServerPartT IO Response
ghcjsSP = dir basepath $ uriRest (serve' ghcjsFileMap)
#ifdef SERVE_DYNAMIC
  where serve' = serveDynamic "../happstack-ghcjs-client/dist/build/happstack-ghcjs-client/happstack-ghcjs-client.jsexe"
#else
  where serve' = serveEmbedded "ghcjs" 
#endif


jsFilePath :: FilePath
[jsFilePath] =  map v fps
  where v = verifyEmbeddedFP "GHCJSWebModule:ghcjsFileMap" ghcjsFileMap
        fps = ["all.js"]
        