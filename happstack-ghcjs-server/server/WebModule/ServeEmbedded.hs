module WebModule.ServeEmbedded (serveEmbedded, verifyEmbeddedFP) where

import Happstack.Server as Happstack (ServerPartT, FilterMonad, guessContentTypeM, mimeTypes, notFound, ok, Response, setHeader, ToMessage(toResponse),dirs, dir, uriRest)
import Data.ByteString as B (ByteString)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup, member)
import Control.Monad.Trans

import System.FilePath

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
