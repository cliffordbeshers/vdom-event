{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (msum)
import Happstack.Server
import Text.Blaze.Html5 as H (Markup, toMarkup)
import Favicon
import WebModule
import ModuleScopeURL

faviconWebSite :: WebSite
faviconWebSite = WebSite { serverpart = faviconHandler favicon
                  , baseURL = [faviconURLMS]
                  , headMarkup = toMarkup ""
                  , bodyMarkup = toMarkup ""
                  , manifest = []
                  }

home :: WebSite
home = WebSite { serverpart = rootHandler $ toMarkup "Hello, world!"
               , baseURL = []
               , headMarkup = return ()
               , bodyMarkup = return ()
               , manifest = []
               }

defaultHandler :: Markup -> ServerPartT IO Response
defaultHandler m = nullDir >> ok (toResponse m)

htmlHandler :: FilePath -> Markup -> ServerPartT IO Response
htmlHandler fp m = dirs fp $ ok (toResponse m)

indexDotHtml :: Markup -> ServerPartT IO Response
indexDotHtml = htmlHandler "index.html"

rootHandler :: Markup -> ServerPartT IO Response
rootHandler m = msum [ defaultHandler m
                     , indexDotHtml m
                     ]

website :: WebSite
website = home `wsum` faviconWebSite

main = do
  let p = 8010
  print ("Serving on localhost",p)
  simpleHTTP (nullConf { port = p }) $ runWebSite website