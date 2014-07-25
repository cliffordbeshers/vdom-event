{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PDFObjectModule where

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

-- TODO these constructors converted to paths are a little weird in the browser
-- Could use boomerang to make the names main.js, etc.

-- Steps to using this module: 
--   Embed Wysihtml5URL in your SiteMap type.
--   Use nestURL
--   Use imports to create some script tags.

data PDFObjectURL = PDFObjectEP EmbeddedPath
                  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo' P.id ''PDFObjectURL)

imports :: Monad m => (PDFObjectURL -> u) -> RouteT u m [WebImport]
imports fu = do
  uj <- showURL (fu $ PDFObjectEP (mkEmbeddedPath jsFilePath))
  return $ [ jscript uj]
  where jscript u = JavaScript $ H.script ! HA.type_ "application/javascript" ! HA.src (toValue u) $ return ()

-- FIXME: the mimetype should be determined statically.

pdfObjectFileMap :: Map FilePath B.ByteString
pdfObjectFileMap = M.fromList $(embedDir "embedded/pdfobject/static")

-- This value incorporates a test that ensures we have the right path at compile time
jsFilePath :: FilePath
jsFilePath = let fp = "pdfobject.js" in
  if M.member fp pdfObjectFileMap
  then fp
  else P.error "pdfobject.js bad path for PDFObject javascript"

route :: (FilterMonad Response (RouteT PDFObjectURL m)) => PDFObjectURL -> RouteT PDFObjectURL m Response
route url =
  case url of
    PDFObjectEP efp -> 
      let mfp = getEmbeddedPath efp in
      case mfp of
        Nothing -> notFound $ toResponse $ show efp
        Just fp ->
          case M.lookup fp pdfObjectFileMap of
            Just bs -> do mt <- guessContentTypeM mimeTypes fp
                          ok $ setHeader "content-type" mt $ toResponse bs
            Nothing -> notFound . toResponse $ "filepath " ++ fp ++ " not found in PDFObjectFileMap"
    
