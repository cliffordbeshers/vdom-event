{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WebModule.BootstrapWebModule (bootstrapModule, BootstrapBindings(..)) where

import WebModule.Markable
import WebModule.ServeEmbedded (serveEmbedded, verifyEmbeddedFP)
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.ModuleScopeURL

import System.FilePath (FilePath, (</>))
import Text.Blaze.Html5 (Markup, toMarkup)
import Happstack.Server as Happstack (ServerPartT, FilterMonad, guessContentTypeM, mimeTypes, notFound, ok, Response, setHeader, ToMessage(toResponse),dirs, dir, uriRest)

import Control.Monad.Trans.Writer
import Data.ByteString as B (ByteString)

import Data.FileEmbed (embedDir)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup, member)
import Text.Blaze.Html5.Attributes as HA (content, href, name, rel, src, type_)
import Web.Routes (RouteT, showURL)
import Web.Routes.TH (derivePathInfo')

-- default(JSNumber, JSString, String)

-- TODO these constructors converted to paths are a little weird in the browser
-- Could use boomerang to make the names main.js, etc.

-- Steps to using this module: 
--   Embed Wysihtml5URL in your SiteMap type.
--   Use nestURL
--   Use imports to create some script tags.


data BootstrapBindings = BootstrapBindings { blue :: Int }

baseurl:: ModuleScopeURL
baseurl = $(moduleScopeURL "")

basepath :: FilePath
basepath = moduleScopeURLtoFilePath baseurl

bootstrapBindings :: BootstrapBindings
bootstrapBindings = BootstrapBindings { blue = 0 }

(+++) :: ModuleScopeURL -> FilePath -> ModuleScopeURL
(+++) = moduleScopeAppend
        

bootstrapModule :: Monad m => WebSiteM m BootstrapBindings
bootstrapModule = wimport ws bootstrapBindings
  where ws :: WebSite
        ws = mzeroWebSite { serverpart = bootstrapSP
                          , headers = bootstrapImports
                          , bodies = [WMB_Initialization "console.log('BootstrapWebModule initialization');"]
                          , baseURL = [baseurl]
                          }

bootstrapImports :: [WM_Header]
bootstrapImports = [ js jsFilePath, css cssFilePath, css themeFilePath]
  where js = WMH_JavaScript . (baseurl +++)
        css = WMH_CSS . (baseurl +++)

-- FIXME: the mimetype should be determined statically.

bootstrapFileMap :: Map FilePath B.ByteString
bootstrapFileMap = M.fromList $(embedDir "embedded/bootstrap")

bootstrapSP :: ServerPartT IO Response
bootstrapSP = dir basepath $ uriRest (serveEmbedded "Bootstrap" bootstrapFileMap)

-- This value incorporates a test that ensures we have the right path at compile time
jsFilePath, cssFilePath, themeFilePath :: FilePath
[jsFilePath, cssFilePath, themeFilePath] =  map v fps
  where v = verifyEmbeddedFP "BootstrapWebModule:bootstrapFileMap" bootstrapFileMap
        fps = ["js/bootstrap.min.js", "css/bootstrap.min.css", "css/bootstrap-theme.min.css" ]


