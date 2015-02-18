{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module WebModule.GHCJSWebModule (ghcjsWebModule, GHCJSBindings(..)) where

import WebModule.Markable
import WebModule.ModuleScopeURL
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.ServeEmbedded
import System.FilePath

#if SERVER
import Happstack.Server
#endif

-- There's really nothing to run, since this will be main.
data GHCJSBindings = GHCJSBindings { start :: Int }

baseurl:: ModuleScopeURL
baseurl = $(moduleScopeURL "")

basepath :: FilePath
basepath = moduleScopeURLtoFilePath baseurl

(+++) :: ModuleScopeURL -> FilePath -> ModuleScopeURL
(+++) = moduleScopeAppend

ghcjsBindings :: GHCJSBindings
ghcjsBindings = GHCJSBindings { start = 1 }

-- Not sure I like this, but it should work.
-- Left indicates dynamic reload of a directory with ghcjs executable
-- Right indicates compile-time, immutable embedding
ghcjsWebModule :: Monad m => Either (FilePath,FilePath) EmbeddedDirectory -> WebSiteM m GHCJSBindings
#if CLIENT
ghcjsWebModule _fileMap = return ghcjsBindings
#else
ghcjsWebModule fileMap = wimport ws ghcjsBindings
  where ws = mzeroWebSite { serverpart = ghcjsSP fileMap
                          , headers = [WMH_JavaScript (baseurl +++ jsFilePath)]
                          , bodies = [WMB_Initialization "console.log('GHCJSWebModule initialization');"]
                          , baseURL = [baseurl]
                          }
        jsFilePath :: FilePath
        jsFilePath = rootname fileMap </> "all.js"
        -- TODO ServeEmbedded.ProvideRootName
        rootname (Left (_,r)) = r
        rootname (Right e) = last . splitDirectories . embeddedPath $ e
#endif

-- TODO WebModule.ServeEmbedded.StaticallyVerifyPaths

#if SERVER
ghcjsSP :: Either (FilePath,FilePath) EmbeddedDirectory -> ServerPartT IO Response
ghcjsSP (Left fpp) = dir basepath $ uriRest (serveDynamic fpp)
ghcjsSP (Right ed) = dir basepath $ uriRest (serveEmbedded ed)
#endif
        
