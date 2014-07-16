{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BootstrapModule where

import Prelude as P
import EmbeddedPath (EmbeddedPath, getEmbeddedPath, mkEmbeddedPath)
import Template (WebImport(..))
import Data.ByteString as B (ByteString)
import Data.Data (Data, Typeable)
import Data.FileEmbed (embedDir)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup, member)
import Happstack.Server (FilterMonad, guessContentTypeM, mimeTypes, notFound, ok, Response, setHeader, ToMessage(toResponse))
import Text.Blaze.Html5 as H ((!), link, meta, script, ToValue(toValue))
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

data BootstrapURL = BootstrapEP EmbeddedPath
                  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo' P.id ''BootstrapURL)

imports :: Monad m => (BootstrapURL -> u) -> RouteT u m [WebImport]
imports fu = do
  uj <- showURL (fu $ BootstrapEP (mkEmbeddedPath jsFilePath))
  uc <- showURL (fu $ BootstrapEP (mkEmbeddedPath cssFilePath))
  ut <- showURL (fu $ BootstrapEP (mkEmbeddedPath themeFilePath))
  return $ [ jscript uj, jcss uc, jcss ut, deviceWidth]
  where jscript u = JavaScript $ H.script ! HA.type_ "application/javascript" ! HA.src (toValue u) $ return ()
        jcss u = Header $ H.link 
                 ! HA.rel "stylesheet"
                 ! HA.type_ "text/css" 
                 ! HA.href (toValue u)
        deviceWidth = Header $ H.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1.0"

-- FIXME: the mimetype should be determined statically.

bootstrapFileMap :: Map FilePath B.ByteString
bootstrapFileMap = M.fromList $(embedDir "embedded/bootstrap")

-- This value incorporates a test that ensures we have the right path at compile time
jsFilePath :: FilePath
jsFilePath = let fp = "js/bootstrap.min.js" in
  if M.member fp bootstrapFileMap
  then fp
  else P.error "Bootstrap.jsFilePath bad path for bootstrap javascript"

-- This value incorporates a test that ensures we have the right path at compile time
cssFilePath :: FilePath
cssFilePath = let fp = "css/bootstrap.min.css" in
  if M.member fp bootstrapFileMap
  then fp
  else P.error "Bootstrap.jsFilePath bad path for bootstrap javascript"

themeFilePath :: FilePath
themeFilePath = let fp = "css/bootstrap-theme.min.css" in
  if M.member fp bootstrapFileMap
  then fp
  else P.error "Bootstrap.jsFilePath bad path for bootstrap javascript"

route :: (FilterMonad Response (RouteT BootstrapURL m)) => BootstrapURL -> RouteT BootstrapURL m Response
route url =
  case url of
    BootstrapEP efp -> 
      let mfp = getEmbeddedPath efp in
      case mfp of
        Nothing -> notFound $ toResponse $ show efp
        Just fp ->
          case M.lookup fp bootstrapFileMap of
            Just bs -> do mt <- guessContentTypeM mimeTypes fp
                          ok $ setHeader "content-type" mt $ toResponse bs
            Nothing -> notFound . toResponse $ "filepath " ++ fp ++ " not found in bootstrapFileMap"
    
