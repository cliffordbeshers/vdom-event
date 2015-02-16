{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (MonadPlus(..))
import Happstack.Server
import WebModule.Favicon
import WebModule.Markable
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.JQueryWebModule
import WebModule.JQueryUIWebModule
import WebModule.BootstrapWebModule
import WebModule.GHCJSWebModule
import WebModule.SortableModule
import WebModule.ServeEmbedded (EmbeddedDirectory, embedDirectoryTH)

faviconWebSite :: WebSite
faviconWebSite = 
  WebSite { serverpart = faviconHandler favicon
          , baseURL = [faviconURLMS]
          , headers = [WMH_Favicon faviconURLMS]
          , bodies = []
          , manifest = []
          }

helloWorld :: Monad m => WebSiteM m ()
helloWorld = tellBody $ [WMB_Initialization "console.log('Hello, World');"]
goodbyeWorld :: Monad m => WebSiteM m ()
goodbyeWorld = tellBody $ [WMB_Initialization "console.log('Goodbye, World');"]


-- Compiling the website should generate an html page that is attached to defaultHandler and indexDotHtml.
-- That html page should include all the headers and bodies.

-- The client generated .js file should be referenced in one of those headers.

-- So I need a website that compiles the client code and the appropriate headers and serverparts.

-- That file should be loaded with Template haskell (server only) and attached to the 
-- defaultHandler and indexDotHtml.

-- This should be compiled with ghcjs, functions that generate dom
-- There should be a path for the client code returned by something that compiles the website

home :: Monad m => WebSiteM m ()
home = helloWorld >> goodbyeWorld

home' :: WebSite
home' = 
  WebSite { serverpart = mzero
          , baseURL = []
          , headers = []
          , bodies = []
          , manifest = []
          }


-- Hard-coded path because template haskell staging, didn't want it in another file.
ghcjsFiles :: Either (FilePath,FilePath) EmbeddedDirectory
#ifdef SERVE_DYNAMIC
ghcjsFiles = Left ("../happstack-ghcjs-client/dist/build/happstack-ghcjs-client", "happstack-ghcjs-client.jsexe")
#else
ghcjsFiles = Right $(embedDirectoryTH "/usr/bin" "happstack-ghcjs-client.jsexe")
-- _ = True == verifyGHCJSFileMap ghcjsFiles
#endif

--website :: WebSite
--website = home `wsum` faviconWebSite

websiteM :: Monad m => WebSiteM m ()
websiteM = do
  mkWebSiteM faviconWebSite
  -- GHCJS is now rolling the .js files in.  Pain.
  GHCJSBindings{..} <- ghcjsWebModule ghcjsFiles
  JQueryBindings{..} <- jQueryModule
  JQueryUIBindings{..} <- jQueryUIModule
  BootstrapBindings{..} <- bootstrapModule
  SortableBindings{..} <- sortableWebModule
  home

main :: IO ()
main = do
  let p = 8010
  print ("Serving on localhost",p)
  (_, ws) <- compileWebSiteM websiteM
  simpleHTTP (nullConf { port = p }) $ serverpart ws
-- TODO
--   simpleHTTP'' (nullConf { port = p }) $ serverpart ws
  
