{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad as Monad (msum)
import Control.Monad.Trans as Trans (liftIO)
import Data.Time.Format (FormatTime(..))
import Happstack.Server
import Happstack.Server.Internal.LogFormat (formatRequestCombined)
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FilePath ((<.>), (</>))
import System.IO (stdout)
import Data.Aeson as Aeson (decode)

debugM x y = putStr x >> putStr ":" >> putStrLn y

main :: IO ()
main = do
  let p = 8010
  debugM "SimpleServer" $ "Starting server on port " ++ show p
  simpleHTTP (nullConf { port = p }) $ handlers
  


handlers :: ServerPartT IO Response
handlers = msum 
           [ nullDir >> ok (toResponse ("plain text" ++ ":" ++ "nullDir"))
           , dirs "index.html" $ ok (toResponse ("plain text" ++ ":" ++ "/index.html"))
           , notFound (toResponse ("plain text" ++ ":" ++ "notFound"))
           ]

-- handlers :: ServerPartT IO Response
-- handlers = do
-- --  path (\s -> ok (toResponse $ show ("plain text", ("path", fromReqURI s :: Maybe String))))
-- --  nullDir >> ok (toResponse ("plain text" ++ ":" ++ "nullDir"))
-- --  anyPath $ ok (toResponse ("plain text" ++ ":" ++ "anyPath"))
--   notFound (toResponse ("plain text" ++ ":" ++ "notFound"))
--   trailingSlash >> ok (toResponse ("plain text" ++ ":" ++ "trailingSlash"))
--   noTrailingSlash >> ok (toResponse ("plain text" ++ ":" ++ "noTrailingSlash"))
--   ok $ toResponse "just plain text"
