{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WebModule.ServeEmbedded (EmbeddedDirectory(..)
  , embedDirectoryTH
#ifndef CLIENT
  , embedDirectory
  , serveDynamic
  , serveEmbedded
  , verifyEmbeddedFP
  , findf
#endif
  ) where

-- TODO WebModule.ServeEmbedded.ProvideRootName
-- in GHCJSWebModule, we have to recompute the rootname, we should provide the original
-- specification from which the embedded directory was produced.

-- TODO WebModule.ServeEmbedded.StaticallyVerifyPaths
-- Modify embeddedDirectoryTH to be completely reliable, this has two parts:
--  1) make arguments to serveEmbeddedTH that represent minimum criteria to be met by the loaded directory,
--  1.5) ideally perhaps running embbededDirectorySetup in ghci which would load and checksum all the files,
--     then cut and paste that into the source code and use System.Directory.Tree fmap checksum and equality,
--     so that we embed only exactly the files that we checked in.
--  2) Whatever the conditions, fail at compile time if they are not met.

import Control.Monad.Trans as Monad (liftIO)
import Data.ByteString.Char8 as B8 (pack, unpack)
import Data.ByteString as B (ByteString, readFile)
import Data.Map as Map (fromList)
import qualified Data.Map as M (lookup)
import System.FilePath (makeRelative, (</>))
import System.Posix
import Language.Haskell.TH

import Language.Haskell.TH.Lift

#if SERVER
import System.Directory.Tree (AnchoredDirTree(..), DirTree(..), flattenDir, readDirectoryWith, zipPaths)
import Happstack.Server as Happstack (guessContentTypeM, mimeTypes, notFound, ok, Response, ServerPartT, setHeader, ToMessage(toResponse))
#endif

type FileMap = [(FilePath, B.ByteString)]

data EmbeddedDirectory = EmbeddedDirectory {
  embeddedPath :: FilePath
  , embeddedMap :: FileMap
  } deriving (Eq, Read, Show)

$(deriveLift ''EmbeddedDirectory)

embedDirectoryTH :: FilePath -> FilePath -> Q Exp -- EmbeddedDirectory
embedDirectoryTH fp d = do
  ed <- runIO $ embedDirectory fp d
  runIO $ printEmbeddedTree ed
  lift ed

printEmbeddedTree :: EmbeddedDirectory -> IO ()
printEmbeddedTree ed = print (embeddedPath ed, map fst (embeddedMap ed))

embedDirectory :: FilePath -> FilePath -> IO EmbeddedDirectory
embedDirectory sdir edir = do
  fs <- findf sdir edir
  return EmbeddedDirectory { embeddedPath = sdir, embeddedMap = fs }
  
#if CLIENT
findf :: FilePath -> FilePath -> IO FileMap
findf _ _ = return []
#else
findf :: FilePath -> FilePath -> IO FileMap
findf cwd sourceDir = fmap convert . readDirectoryWith B.readFile $ (cwd </> sourceDir)
  where convert = map file . filter byFile . flattenDir . zipPaths . reanchor ""
        -- Directory trees have weird types.  zipPaths breaks the functor model.
        -- convert2 = fold . map listify . zipPaths
        -- listify = (:[])

reanchor :: FilePath -> AnchoredDirTree a -> AnchoredDirTree a
reanchor a t = t { anchor = a }

byFile :: DirTree a -> Bool
byFile (File _ _) = True
byFile _ = False

serveEmbedded :: EmbeddedDirectory -> FilePath -> ServerPartT IO Response
serveEmbedded edir fpa =
  -- fpa is filepath absolute, it should be a filepath found in the embedded directory
  -- but it has a leading /, which we need to remove
  let fp = "/" `makeRelative` fpa -- drop the leading /
      filemap = Map.fromList (embeddedMap edir) in
  do    
    case M.lookup fp filemap of
      Just bs -> do mt <- guessContentTypeM mimeTypes fp
                    liftIO (getWorkingDirectory >>= (\cwd -> putStrLn $ "ServeEmbedded.hs embedded cwd = " ++ cwd))
                    liftIO $ putStrLn $ "ServeEmbedded.hs fpa = " ++ fpa
                    ok $ setHeader "content-type" mt $ toResponse bs
      Nothing -> notFound . toResponse $ "filepath " ++ fp ++ " not found in EmbeddedDirectory create from " ++ (embeddedPath edir)

serveDynamic :: (FilePath, FilePath) -> FilePath -> ServerPartT IO Response
serveDynamic (parent,edir) uri = do
  -- edir is the directory to embed, parent is the path to edir.
  liftIO $ print $ "serveDynamic:" ++ parent ++ " " ++ edir
  ed <- liftIO $ embedDirectory parent edir
  serveEmbedded ed uri
#endif

verifyEmbeddedFP :: EmbeddedDirectory -> FilePath -> FilePath
verifyEmbeddedFP ed fp =
  if elem fp (fst . unzip $ embeddedMap ed)
  then fp
  else error $ "ServeEmbedded:verifyEmbbeddedFP -- " ++ (embeddedPath ed) ++ " does not contain path " ++ fp

instance Lift ByteString where
  lift = bsToExp

-- from Data.FileEmbed
-- why is there no public instance?
bsToExp :: ByteString -> Q Exp
bsToExp bs = do
    helper <- [| stringToBs |]
    let chars = B8.unpack bs
    return $! AppE helper $! LitE $! StringL chars

stringToBs :: String -> B.ByteString
stringToBs = B8.pack
  
