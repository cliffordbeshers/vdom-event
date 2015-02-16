{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WebModule.ServeEmbedded (EmbeddedDirectory(..)
#ifndef CLIENT
  , embedDirectory
  , embedDirectoryTH
  , serveDynamic
  , serveEmbedded
  , verifyEmbeddedFP
  , findf
#endif
  ) where

#if SERVER
import Control.Monad.Trans as Monad (liftIO)
import Data.ByteString.Char8 as B8 (pack, unpack)
import Data.ByteString as B (ByteString, readFile)
import Data.Map as Map (fromList)
import qualified Data.Map as M (lookup)
import Happstack.Server as Happstack (guessContentTypeM, mimeTypes, notFound, ok, Response, ServerPartT, setHeader, ToMessage(toResponse))
import System.FilePath (makeRelative, (</>))
import System.Directory.Tree
import System.Posix
import Language.Haskell.TH

import Language.Haskell.TH.Lift
#endif

#ifndef CLIENT
type FileMap = [(FilePath, B.ByteString)]
#endif

data EmbeddedDirectory = EmbeddedDirectory {
#ifndef CLIENT
  embeddedPath :: FilePath
  , embeddedMap :: FileMap
#endif
  } deriving (Eq, Read, Show)

#ifndef CLIENT
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
#endif
  
