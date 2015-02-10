{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WebModule.ServeEmbedded (EmbeddedDirectory -- opaque
#if SERVER
  , embedDirectory
  , embedDirectoryTH
  , serveDynamic
  , serveEmbedded
  , verifyEmbeddedFP
#endif
  ) where

#if SERVER
import Control.Monad.Trans as Monad (liftIO)
import Data.ByteString.Char8 as B8 (pack, unpack)
import Data.ByteString as B (ByteString, readFile)
import Data.Map as Map (fromList)
import qualified Data.Map as M (lookup)
import Happstack.Server as Happstack (guessContentTypeM, mimeTypes, notFound, ok, Response, ServerPartT, setHeader, ToMessage(toResponse))
import System.FilePath (makeRelative)
import System.Directory.Tree
import System.Posix
import Language.Haskell.TH

import Language.Haskell.TH.Lift
#endif

#if SERVER
type FileMap = [(FilePath, B.ByteString)]
#endif

data EmbeddedDirectory = EmbeddedDirectory {
#if SERVER
  embeddedPath :: FilePath
  , embeddedMap :: FileMap
#endif
  } deriving (Eq, Read, Show)

#if SERVER
$(deriveLift ''EmbeddedDirectory)

embedDirectoryTH :: FilePath -> Q Exp -- EmbeddedDirectory
embedDirectoryTH fp = do
  ed <- runIO $ embedDirectory fp
  lift ed

embedDirectory :: FilePath -> IO EmbeddedDirectory
embedDirectory sourceDir = do
  fs <- findf sourceDir
  return EmbeddedDirectory { embeddedPath = sourceDir, embeddedMap = fs }
  
findf :: FilePath -> IO FileMap
findf = fmap convert . readDirectoryWith B.readFile
  where convert = map file . filter byFile . flattenDir . zipPaths
        -- Directory trees have weird types.  zipPaths breaks the functor model.
        -- convert2 = fold . map listify . zipPaths
        -- listify = (:[])
        byFile (File _ _) = True
        byFile _ = False


serveEmbedded :: EmbeddedDirectory -> FilePath -> ServerPartT IO Response
serveEmbedded edir fpa =
  let fp = "/" `makeRelative` fpa
      filemap = Map.fromList (embeddedMap edir) in
  do    
    case M.lookup fp filemap of
      Just bs -> do mt <- guessContentTypeM mimeTypes fp
                    liftIO (getWorkingDirectory >>= (\cwd -> putStrLn $ "ServeEmbedded.hs embedded cwd = " ++ cwd))
                    liftIO $ putStrLn $ "ServeEmbedded.hs fpa = " ++ fpa
                    ok $ setHeader "content-type" mt $ toResponse bs
      Nothing -> notFound . toResponse $ "filepath " ++ fp ++ " not found in EmbeddedDirectory create from " ++ (embeddedPath edir)

serveDynamic :: FilePath -> FilePath -> ServerPartT IO Response
serveDynamic sourceDir fpa = do
  ed <- liftIO $ embedDirectory sourceDir
  serveEmbedded ed fpa

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
  
