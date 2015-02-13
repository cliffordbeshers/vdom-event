{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module WebModule.JQueryUIWebModule (jQueryUIModule, JQueryUIBindings(..)) where

#if SERVER
import Happstack.Server as Happstack (dir, Response, ServerPartT, uriRest)
import WebModule.Markable (WM_Body(WMB_Initialization), WM_Header(WMH_CSS, WMH_JavaScript))
import WebModule.ModuleScopeURL (moduleScopeAppend, moduleScopeURL, ModuleScopeURL, moduleScopeURLtoFilePath)
import WebModule.ServeEmbedded (embedDirectoryTH, serveEmbedded, verifyEmbeddedFP, EmbeddedDirectory)
import WebModule.WebModule (WebSite(baseURL, bodies, headers, serverpart))
import WebModule.WebModuleM (mzeroWebSite, wimport)
#endif
import WebModule.WebModuleM (WebSiteM)
import System.FilePath ((</>))


data JQueryUIBindings = JQueryUIBindings { sortable :: Int }


-- Do Not Export
jQueryUIBindings :: JQueryUIBindings
jQueryUIBindings = JQueryUIBindings { sortable = 1}

jQueryUIModule :: Monad m => WebSiteM m JQueryUIBindings
#if CLIENT
jQueryUIModule = return jQueryUIBindings
#else
jQueryUIModule = wimport ws jQueryUIBindings
  where ws = mzeroWebSite { serverpart = jQueryUISP
                          , headers = [ WMH_CSS (baseurl +++ cssFilePath)
                                      , WMH_CSS (baseurl +++ themeFilePath)
                                      , WMH_JavaScript (baseurl +++ jsFilePath)
                                      ] 
                          , bodies = [WMB_Initialization "console.log('jQueryUIWebModule initialization');"]
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


jQueryUISP :: ServerPartT IO Response
jQueryUISP = dir basepath $ uriRest (serveEmbedded jQueryUIFileMap)

-- FIXME: the mimetype should be determined statically.

jQueryUIFileMap :: EmbeddedDirectory
jQueryUIFileMap = $(embedDirectoryTH "embedded/WebModule/JQueryUIWebModule/jquery-ui-1.11.2")


-- This value incorporates a test that ensures we have the right path at compile time
jsFilePath, cssFilePath, themeFilePath:: FilePath
[jsFilePath, cssFilePath, themeFilePath] =  map v fps
  where v = verifyEmbeddedFP jQueryUIFileMap
        fps = map ("jquery-ui-1.11.2" </>) ["jquery-ui.js", "jquery-ui.css", "jquery-ui.theme.css"]
#endif
