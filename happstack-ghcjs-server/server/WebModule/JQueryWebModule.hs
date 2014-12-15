{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module WebModule.JQueryWebModule (jQueryModule, JQueryBindings(..)) where

#if CLIENT
import qualified GHCJS.JQuery as JQuery (on, Event, EventType, HandlerSettings, JQuery)
#else
import qualified GHCJSStub.JQuery as JQuery (on, Event, EventType, HandlerSettings, JQuery)
#endif
import WebModule.Markable
import WebModule.ServeEmbedded (serveEmbedded, verifyEmbeddedFP)
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.ModuleScopeURL
import Happstack.Server as Happstack (ServerPartT, Response,  dir, uriRest)

import Data.FileEmbed (embedDir)
import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Data.ByteString as B (ByteString)


data JQueryBindings = JQueryBindings { on :: (JQuery.Event -> IO ()) -> JQuery.EventType -> JQuery.HandlerSettings -> JQuery.JQuery -> IO (IO ()) }


baseurl:: ModuleScopeURL
baseurl = $(moduleScopeURL "")

basepath :: FilePath
basepath = moduleScopeURLtoFilePath baseurl

(+++) :: ModuleScopeURL -> FilePath -> ModuleScopeURL
(+++) = moduleScopeAppend

jQueryModule :: Monad m => WebSiteM m JQueryBindings
jQueryModule = wimport ws jQueryBindings
  where ws = mzeroWebSite { serverpart = jQuerySP
                          , headers = [WMH_JavaScript (baseurl +++ jsFilePath)] 
                          , bodies = [WMB_Initialization "console.log('JQueryWebModule initialization');"]
                          , baseURL = [baseurl]
                          }
jQuerySP :: ServerPartT IO Response
jQuerySP = dir basepath $ uriRest (serveEmbedded "jQuery" jQueryFileMap)

-- Do Not Export
jQueryBindings :: JQueryBindings
jQueryBindings = JQueryBindings { on = JQuery.on }


-- FIXME: the mimetype should be determined statically.

jQueryFileMap :: Map FilePath B.ByteString
jQueryFileMap = M.fromList $(embedDir "embedded/jquery")

--       let mfp = getEmbeddedPath efp in
--       case mfp of
--         Nothing -> notFound $ toResponse $ show efp
--         Just fp ->
--           case M.lookup fp bootstrapFileMap of
--             Just bs -> do mt <- guessContentTypeM mimeTypes fp
--                           ok $ setHeader "content-type" mt $ toResponse bs
--             Nothing -> notFound . toResponse $ "filepath " ++ fp ++ " not found in bootstrapFileMap"


-- This value incorporates a test that ensures we have the right path at compile time
jsFilePath :: FilePath
[jsFilePath] =  map v fps
  where v = verifyEmbeddedFP "JQueryWebModule:jQueryFileMap" jQueryFileMap
        fps = ["jquery-1.11.0.min.js"]


-- Not using WebRoutes yet, not sure I can get it to cooperate with ghcjs.

-- route :: (FilterMonad Response (RouteT BootstrapURL m)) => BootstrapURL -> RouteT BootstrapURL m Response
-- route url =
--   case url of
--     BootstrapEP efp -> 
--       let mfp = getEmbeddedPath efp in
--       case mfp of
--         Nothing -> notFound $ toResponse $ show efp
--         Just fp ->
--           case M.lookup fp bootstrapFileMap of
--             Just bs -> do mt <- guessContentTypeM mimeTypes fp
--                           ok $ setHeader "content-type" mt $ toResponse bs
--             Nothing -> notFound . toResponse $ "filepath " ++ fp ++ " not found in bootstrapFileMap"
    
