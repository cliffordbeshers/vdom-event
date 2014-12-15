{-# LANGUAGE TemplateHaskell #-}
module WebModule.JQueryUIWebModule (jQueryUIModule, JQueryUIBindings(..)) where

import Data.ByteString as B (ByteString)
import Data.FileEmbed (embedDir)
import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Happstack.Server as Happstack (dir, Response, ServerPartT, uriRest)
import WebModule.Markable (WM_Body(WMB_Initialization), WM_Header(WMH_CSS, WMH_JavaScript))
import WebModule.ModuleScopeURL (moduleScopeAppend, moduleScopeURL, ModuleScopeURL, moduleScopeURLtoFilePath)
import WebModule.ServeEmbedded (serveEmbedded, verifyEmbeddedFP)
import WebModule.WebModule (WebSite(baseURL, bodies, headers, serverpart))
import WebModule.WebModuleM (mzeroWebSite, WebSiteM, wimport)


data JQueryUIBindings = JQueryUIBindings { sortable :: Int }


baseurl:: ModuleScopeURL
baseurl = $(moduleScopeURL "")

basepath :: FilePath
basepath = moduleScopeURLtoFilePath baseurl

(+++) :: ModuleScopeURL -> FilePath -> ModuleScopeURL
(+++) = moduleScopeAppend

jQueryUIModule :: Monad m => WebSiteM m JQueryUIBindings
jQueryUIModule = wimport ws jQueryUIBindings
  where ws = mzeroWebSite { serverpart = jQueryUISP
                          , headers = [WMH_JavaScript (baseurl +++ jsFilePath), WMH_CSS (baseurl +++ cssFilePath)] 
                          , bodies = [WMB_Initialization "console.log('jQueryUIWebModule initialization');"]
                          , baseURL = [baseurl]
                          }
jQueryUISP :: ServerPartT IO Response
jQueryUISP = dir basepath $ uriRest (serveEmbedded "jQueryUI" jQueryUIFileMap)

-- Do Not Export
jQueryUIBindings :: JQueryUIBindings
jQueryUIBindings = JQueryUIBindings { sortable = 1}

-- FIXME: the mimetype should be determined statically.

jQueryUIFileMap :: Map FilePath B.ByteString
jQueryUIFileMap = M.fromList $(embedDir "embedded/jquery-ui-1.11.2")


-- This value incorporates a test that ensures we have the right path at compile time
jsFilePath, cssFilePath :: FilePath
[jsFilePath, cssFilePath] =  map v fps
  where v = verifyEmbeddedFP "JQueryUIWebModule:jQueryUIFileMap" jQueryUIFileMap
        fps = ["jquery-ui.min.js", "jquery-ui.min.css"]

