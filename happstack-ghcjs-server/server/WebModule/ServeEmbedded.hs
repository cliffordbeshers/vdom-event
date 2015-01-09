{-# LANGUAGE CPP #-}
#if CLIENT
module WebModule.ServeEmbedded() where
#endif

#if SERVER
module WebModule.ServeEmbedded (serveDynamic, serveEmbedded, verifyEmbeddedFP) where

import Control.Monad.Trans as Monad (liftIO)
import Data.ByteString as B (ByteString, readFile)
import Data.Map (Map)
import qualified Data.Map as M (lookup, member)
import Happstack.Server as Happstack (guessContentTypeM, mimeTypes, notFound, ok, Response, ServerPartT, setHeader, ToMessage(toResponse))
import System.FilePath (makeRelative, (</>))
import System.Posix

serveEmbedded :: String -> Map FilePath B.ByteString -> FilePath -> ServerPartT IO Response
serveEmbedded filemapname filemap fpa = do
  let fp = "/" `makeRelative` fpa
  -- liftIO . putStrLn $ "serveEmbedded:Map.lookup \""++ filemapname ++ "\" " ++ fp ++ "= " ++ (show $ M.lookup fp filemap)
  case M.lookup fp filemap of
    Just bs -> do mt <- guessContentTypeM mimeTypes fp
                  liftIO (getWorkingDirectory >>= (\cwd -> putStrLn $ "ServeEmbedded.hs embedded cwd = " ++ cwd))
                  liftIO $ putStrLn $ "ServeEmbedded.hs fpa = " ++ fpa
                  ok $ setHeader "content-type" mt $ toResponse bs
    Nothing -> notFound . toResponse $ "filepath " ++ fp ++ " not found in filemap " ++ filemapname

serveDynamic :: String -> Map FilePath B.ByteString -> FilePath -> ServerPartT IO Response
serveDynamic filemapname filemap fpa = do
  let keyfp = "/" `makeRelative` fpa
  let dynfp = filemapname </> keyfp
  -- liftIO . putStrLn $ "serveEmbedded:Map.lookup \""++ filemapname ++ "\" " ++ keyfp ++ "= " ++ (show $ M.lookup keyfp filemap)
  case M.lookup keyfp filemap of
    Just bs -> do mt <- guessContentTypeM mimeTypes keyfp
                  liftIO (getWorkingDirectory >>= (\cwd -> putStrLn $ "ServeEmbedded.hs dynamic cwd = " ++ cwd))
                  liftIO $ putStrLn $ "ServeEmbedded.hs dynfp = " ++ dynfp
                  bs' <- liftIO $ B.readFile dynfp
                  ok $ setHeader "content-type" mt $ toResponse bs'
    Nothing -> notFound . toResponse $ "filepath " ++ fpa ++ " not found in filemap " ++ filemapname


verifyEmbeddedFP :: String -> Map FilePath B.ByteString -> FilePath -> FilePath
verifyEmbeddedFP filemapname filemap fp =
  if M.member fp filemap
  then fp
  else error $ "ServeEmbedded:verifyEmbbeddedFP -- " ++ filemapname ++ " does not contain path " ++ fp
#endif