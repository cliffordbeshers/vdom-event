{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WebModule.BootstrapWebModule (bootstrapModule, BootstrapBindings(..)) where

#if SERVER
import Happstack.Server as Happstack (dir, Response, ServerPartT, uriRest)
import WebModule.Markable (WM_Body(WMB_Initialization), WM_Header(WMH_CSS, WMH_JavaScript))
import WebModule.ModuleScopeURL (moduleScopeAppend, moduleScopeURL, ModuleScopeURL, moduleScopeURLtoFilePath)
import WebModule.ServeEmbedded (embedDirectoryTH, serveEmbedded, verifyEmbeddedFP, EmbeddedDirectory)
import WebModule.WebModule (WebSite(baseURL, bodies, headers, serverpart))
import WebModule.WebModuleM (mzeroWebSite, WebSiteM, wimport)
#endif

-- default(JSNumber, JSString, String)

-- TODO these constructors converted to paths are a little weird in the browser
-- Could use boomerang to make the names main.js, etc.

-- Steps to using this module: 
--   Embed Wysihtml5URL in your SiteMap type.
--   Use nestURL
--   Use imports to create some script tags.


data BootstrapBindings = BootstrapBindings { blue :: Int }

bootstrapBindings :: BootstrapBindings
bootstrapBindings = BootstrapBindings { blue = 0 }

bootstrapModule :: Monad m => WebSiteM m BootstrapBindings
#if CLIENT
bootstrapModule = return bootstrapBindings
#else
bootstrapModule = wimport ws bootstrapBindings
  where ws :: WebSite
        ws = mzeroWebSite { serverpart = bootstrapSP
                          , headers = bootstrapImports
                          , bodies = [WMB_Initialization "console.log('BootstrapWebModule initialization');"]
                          , baseURL = [baseurl]
                          }
#endif

#if SERVER
baseurl:: ModuleScopeURL
baseurl = $(moduleScopeURL "")

basepath :: FilePath
basepath = moduleScopeURLtoFilePath baseurl

(+++) :: ModuleScopeURL -> FilePath -> ModuleScopeURL
(+++) = moduleScopeAppend
        

bootstrapImports :: [WM_Header]
bootstrapImports = [ js jsFilePath, css cssFilePath, css themeFilePath]
  where js = WMH_JavaScript . (baseurl +++)
        css = WMH_CSS . (baseurl +++)

-- FIXME: the mimetype should be determined statically.

bootstrapFileMap :: EmbeddedDirectory
bootstrapFileMap = $(embedDirectoryTH "embedded/WebModule/BootstrapWebModule/bootstrap")

bootstrapSP :: ServerPartT IO Response
bootstrapSP = dir basepath $ uriRest (serveEmbedded bootstrapFileMap)

-- This value incorporates a test that ensures we have the right path at compile time
jsFilePath, cssFilePath, themeFilePath :: FilePath
[jsFilePath, cssFilePath, themeFilePath] =  map v fps
  where v = verifyEmbeddedFP bootstrapFileMap
        fps = ["js/bootstrap.min.js", "css/bootstrap.min.css", "css/bootstrap-theme.min.css" ]

#endif
