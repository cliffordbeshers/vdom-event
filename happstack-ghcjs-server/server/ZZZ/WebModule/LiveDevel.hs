{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module WebModule.LiveDevel (imports) where

import Prelude as P
import Data.ByteString as B (ByteString)
import Template (WebImport(..))
import Data.List (intercalate)
import Text.Blaze.Html5 as H ((!), script)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA (type_, src)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Routes (RouteT)
import "mtl" Control.Monad.Trans (MonadIO, liftIO)
import System.IO (readFile)
import System.Directory
import System.FilePath
import Data.FileEmbed (embedFile)
import Data.ByteString.Char8 as B8 (unpack)
import BasePath

default (String)

wysihtml5wrangler :: String
wysihtml5wrangler = B8.unpack $(embedFile ("embedded/livedevel" </> "wysihtml5wrangler.js"))

livedeveljs :: FilePath
livedeveljs = basepath </> "livedevel.js"

livedevelcss :: FilePath
livedevelcss = basepath </> "livedevel.css"

-- This does not need to be a RouteT, but most imports are, so I'm keeping that form.
imports :: MonadIO m => RouteT u m [WebImport]
imports = do
  livejs <- liftIO $ loadJS livedeveljs
  livecss <- liftIO $ loadCSS livedevelcss
  return $ [ jscript wysihtml5wrangler] ++ livejs ++ livecss

loadJS :: FilePath -> IO [WebImport] 
loadJS path = do
  -- Unfortunately, I don't know the source path at run time, since we switch to /srv/...
  e <- liftIO $ doesFileExist path
  if not e
    then return []
    else do js <- readFile path
            return [jscript js]
              
loadCSS :: FilePath -> IO [WebImport] 
loadCSS path = do
  -- Unfortunately, I don't know the source path at run time, since we switch to /srv/...
  e <- liftIO $ doesFileExist path
  if not e
    then return []
    else do css <- readFile path
            return [inlinecss css]
              
-- jscript :: ToMarkup a => a -> WebImport
jscript u = JavaScript $ H.script ! HA.type_ "application/javascript" $ H.preEscapedToHtml u
inlinecss u = Header $ H.style ! HA.type_ "text/css" $ toMarkup u