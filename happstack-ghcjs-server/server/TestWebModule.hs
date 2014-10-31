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

rootHandler :: Markup -> ServerPartT IO Response
rootHandler m = msum [ nullDir >> ok (toResponse m)
                     , dirs "index.html" $ ok (toResponse m)
                     ]

website :: WebSite
website = home `wsum` faviconWebSite

main = do
  let p = 8010
  print ("Serving on localhost",p)
  simpleHTTP (nullConf { port = p }) $ runWebSite website