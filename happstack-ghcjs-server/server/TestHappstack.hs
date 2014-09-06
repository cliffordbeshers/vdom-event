{-# LANGUAGE TemplateHaskell #-}
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
import Manifest
import Template
import Data.Text as Text
import Data.FileEmbed
import Data.ByteString as B (ByteString)

debugM x y = putStr x >> putStr ":" >> putStrLn y

main :: IO ()
main = do
  let p = 8010
  debugM "SimpleServer" $ "Starting server on port " ++ show p
  simpleHTTP (nullConf { port = p }) $ handlers
  


handlers :: ServerPartT IO Response
handlers = msum [ rootHandler
                , manifestHandler [ WSE $ Text.pack "/manifest.appcache" 
                                  , WSE $ Text.pack "index.html"
                                  , WSE $ Text.pack "favicon.ico"
                                  ]
                , faviconHandler favicon
                , notFound (toResponse ("plain text" ++ ":" ++ "notFound"))
                ]


application = htmlTemplate (Text.pack "Test Happstack") [] []

rootHandler :: ServerPartT IO Response
rootHandler = msum [ nullDir >> ok (toResponse application)
                   , dirs "index.html" $ ok (toResponse application)
                   ]

manifestHandler :: WS -> ServerPartT IO Response
manifestHandler ws = dirs "manifest.appcache" $ ok $ setMimeType $ toResponse (genManifest ws)
  where setMimeType = setHeader "Content-Type" manifestMimeType
        manifestMimeType = "text/cache-manifest"

favicon :: B.ByteString
favicon = $(embedFile "server/favicon.ico")

faviconHandler :: B.ByteString -> ServerPartT IO Response
faviconHandler favicon = dirs "/favicon.ico" $ ok $ setMimeType $ toResponse favicon
  where setMimeType = setHeader "Content-Type" manifestMimeType
        manifestMimeType = "text/cache-manifest"

