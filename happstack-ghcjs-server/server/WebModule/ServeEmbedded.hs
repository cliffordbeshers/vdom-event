module WebModule.ServeEmbedded (serveEmbedded, verifyEmbeddedFP) where

import Data.ByteString as B (ByteString)
import Data.Map (Map)
import qualified Data.Map as M (lookup, member)
import Happstack.Server as Happstack (guessContentTypeM, mimeTypes, notFound, ok, Response, ServerPartT, setHeader, ToMessage(toResponse))
import System.FilePath (makeRelative)

serveEmbedded :: String -> Map FilePath B.ByteString -> FilePath -> ServerPartT IO Response
serveEmbedded filemapname filemap fpa = do
  let fp = "/" `makeRelative` fpa
  -- liftIO . putStrLn $ "serveEmbedded:Map.lookup \""++ filemapname ++ "\" " ++ fp ++ "= " ++ (show $ M.lookup fp filemap)
  case M.lookup fp filemap of
    Just bs -> do mt <- guessContentTypeM mimeTypes fp
                  ok $ setHeader "content-type" mt $ toResponse bs
    Nothing -> notFound . toResponse $ "filepath " ++ fp ++ " not found in filemap " ++ filemapname


verifyEmbeddedFP :: String -> Map FilePath B.ByteString -> FilePath -> FilePath
verifyEmbeddedFP filemapname filemap fp =
  if M.member fp filemap
  then fp
  else error $ "ServeEmbedded:verifyEmbbeddedFP -- " ++ filemapname ++ " does not contain path " ++ fp
