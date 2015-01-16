{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module NoEscape (NoEscape(..), getEscapePathSafe, route) where
import Control.Applicative (Alternative(many))
import Control.Monad (MonadPlus)
import "mtl" Control.Monad.Trans (MonadIO)
import Data.Data (Data, Typeable)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Happstack.Server (FilterMonad, guessContentTypeM, mimeTypes, notFound, Response, serveFile, ServerMonad, ToMessage(toResponse))
import Happstack.Server.FileServe.BuildingBlocks (isSafePath)
import System.FilePath ((</>), joinPath, makeRelative, splitDirectories)
import Web.Routes (anySegment, PathInfo(..))
import Prelude

-- This module has the magic to get the rest of the URL trailing off a
-- web routes URL.  This makes it possible to use web routes to serve
-- web packages (e.g., bootstrap) that have embedded relative
-- links to resources such as css files, images, etc.

-- We do not know for sure if this is safe or not.

-- These are the essential components.

newtype NoEscape a = NoEscape a
      deriving (Eq, Ord, Data, Typeable, Read, Show)

instance PathInfo (NoEscape String) where
    toPathSegments (NoEscape s) = map pack $ splitDirectories s
    fromPathSegments =
        do ps <- many anySegment
           return (NoEscape (joinPath $ map unpack ps))

getEscapePathSafe :: NoEscape FilePath -> Maybe FilePath
getEscapePathSafe (NoEscape fp) = 
  let fp' = makeRelative "/" fp in 
  if isSafePath (splitDirectories fp')
  then Just fp'
  else Nothing

-- This show an example usage.

data ClckURL  = ThemeDataNoEscape (NoEscape FilePath) deriving Show


-- route' :: (MonadPlus m, ServerMonad m,
--            FilterMonad Response m, MonadIO m) =>
--           ClckURL -> m Response
-- route' url = 
--   case url of
--     (ThemeDataNoEscape (NoEscape fp'))  -> do
--       let fp'' = makeRelative "/" fp'
--       if not (isSafePath (splitDirectories fp''))
--         then notFound (toResponse ())
--         else serveFile (guessContentTypeM mimeTypes) ("base" </> fp'')


-- route :: FilePath -> ClckURL -> ServerPartT IO Response
route :: (MonadPlus m, ServerMonad m, 
          FilterMonad Response m, MonadIO m) =>
         FilePath -> ClckURL -> m Response
route base url = 
  case url of
    (ThemeDataNoEscape p)  -> 
      fromMaybe (err url) (fmap serve $ getEscapePathSafe p)
        where err u = notFound (toResponse $ show u)
              serve fp = serveFile (guessContentTypeM mimeTypes) (base </> fp)
