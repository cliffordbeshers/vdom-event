{-# LANGUAGE TemplateHaskell #-}
module JQueryUIWebModule (jQueryUIModule, JQueryUIBindings(..)) where

import Control.Monad.Trans (liftIO)
import Markable
import ServeEmbedded (serveEmbedded, verifyEmbeddedFP)
import WebModule
import WebModuleM
import ModuleScopeURL
import Text.Blaze.Html5 (Markup, toMarkup)
import Happstack.Server as Happstack (ServerPartT, FilterMonad, guessContentTypeM, mimeTypes, notFound, ok, Response, setHeader, ToMessage(toResponse),dirs, dir, uriRest)

import Control.Monad.Trans.Writer
import Data.FileEmbed (embedDir)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup, member)
import Data.ByteString as B (ByteString)
import System.FilePath (makeRelative)


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
                          , bodies = [WMB_Initialization "jQueryUI initialization"]
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
jsFilePath :: FilePath
[jsFilePath, cssFilePath] =  map v fps
  where v = verifyEmbeddedFP "JQueryUIWebModule:jQueryUIFileMap" jQueryUIFileMap
        fps = ["jquery-ui.min.js", "jquery-ui.min.css"]

