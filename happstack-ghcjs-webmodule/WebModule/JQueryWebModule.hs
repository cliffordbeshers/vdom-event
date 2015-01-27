{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module WebModule.JQueryWebModule (jQueryModule, JQueryBindings(..)) where

#if CLIENT
import qualified JavaScript.JQuery as JQuery (on, Event, EventType, HandlerSettings, JQuery)
#else
import qualified WebModule.GHCJSStub.JQuery as JQuery (on, Event, EventType, HandlerSettings, JQuery)
#endif

#if CLIENT
import WebModule.WebModuleM (WebSiteM)
#else
import WebModule.WebModuleM (WebSiteM, wimport, mzeroWebSite)
#endif

#if SERVER
import WebModule.Markable
import WebModule.ServeEmbedded (serveEmbedded, verifyEmbeddedFP)
import WebModule.WebModule
import WebModule.ModuleScopeURL
import Happstack.Server as Happstack (ServerPartT, Response,  dir, uriRest)
import Data.FileEmbed (embedDir)
import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Data.ByteString as B (ByteString)
#endif


data JQueryBindings = JQueryBindings { on :: (JQuery.Event -> IO ()) -> JQuery.EventType -> JQuery.HandlerSettings -> JQuery.JQuery -> IO (IO ()) }

-- Do Not Export
jQueryBindings :: JQueryBindings
jQueryBindings = JQueryBindings { on = JQuery.on }

#if CLIENT
jQueryModule :: Monad m => WebSiteM m JQueryBindings
jQueryModule = return jQueryBindings
#else
jQueryModule :: Monad m => WebSiteM m JQueryBindings
jQueryModule = wimport ws jQueryBindings
  where ws = mzeroWebSite { serverpart = jQuerySP
                          , headers = [WMH_JavaScript (baseurl +++ jsFilePath)] 
                          , bodies = [WMB_Initialization "console.log('JQueryWebModule initialization');"]
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

jQuerySP :: ServerPartT IO Response
jQuerySP = dir basepath $ uriRest (serveEmbedded "jQuery" jQueryFileMap)

-- FIXME: the mimetype should be determined statically.

jQueryFileMap :: Map FilePath B.ByteString
jQueryFileMap = M.fromList $(embedDir "embedded/WebModule/JQueryWebModule/jquery")


-- This value incorporates a test that ensures we have the right path at compile time
jsFilePath :: FilePath
[jsFilePath] =  map v fps
  where v = verifyEmbeddedFP "JQueryWebModule:jQueryFileMap" jQueryFileMap
        fps = ["jquery-1.11.0.min.js"]
#endif
