{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module EmbeddedPath ( EmbeddedPath
                    , mkEmbeddedPath
                    , getEmbeddedPath
                    , route
                    )
       where

import Prelude
import NoEscape (getEscapePathSafe, NoEscape(..))
import Control.Applicative (Alternative(many))
import Control.Monad (MonadPlus)
import "mtl" Control.Monad.Trans (MonadIO)
import Data.Data (Data, Typeable)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Happstack.Server (FilterMonad, guessContentTypeM, mimeTypes, notFound, Response, serveFile, ServerMonad, ToMessage(toResponse))
import System.FilePath ((</>), joinPath, splitDirectories)
import Web.Routes (anySegment, PathInfo(..))

-- This module has the magic to get the rest of the URL trailing off a
-- web routes URL.  This makes it possible to use web routes to serve
-- web packages (e.g., bootstrap) that have embedded relative
-- links to resources such as css files, images, etc.

-- We do not know for sure if this is safe or not.

-- These are the essential components.

newtype EmbeddedPath = EmbeddedPath (NoEscape FilePath)
                     deriving (Eq, Ord, Data, Typeable, Read, Show)

instance PathInfo EmbeddedPath where
    toPathSegments (EmbeddedPath (NoEscape s)) = map pack $ splitDirectories s
    fromPathSegments =
        do ps <- many anySegment
           return (EmbeddedPath (NoEscape (joinPath $ map unpack ps)))

mkEmbeddedPath :: FilePath -> EmbeddedPath
mkEmbeddedPath = EmbeddedPath . NoEscape

getEmbeddedPath :: EmbeddedPath -> Maybe FilePath
getEmbeddedPath (EmbeddedPath nefp) = getEscapePathSafe nefp

-- This show an example usage.

data ClckURL  = ThemeData EmbeddedPath deriving Show

-- route :: FilePath -> ClckURL -> ServerPartT IO Response
route :: (MonadPlus m, ServerMonad m, 
          FilterMonad Response m, MonadIO m) =>
         FilePath -> ClckURL -> m Response
route base url = 
  case url of
    (ThemeData ep)  -> 
      fromMaybe (err url) (fmap serve $ getEmbeddedPath ep)
        where err u = notFound (toResponse $ show u)
              serve fp = serveFile (guessContentTypeM mimeTypes) (base </> fp)
