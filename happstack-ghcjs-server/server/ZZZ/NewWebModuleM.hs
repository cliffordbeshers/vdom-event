{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module WebModuleM where

import Control.Applicative
import Control.Monad as Monad
import Control.Monad.Trans.Writer
import Markable
import WebModule
import Happstack.Server as Happstack (ServerPartT, Response, nullDir, ok, toResponse, dirs)
import Text.Blaze.Html5 (Markup, toMarkup)
import Data.Lens.Strict
import Template
import Data.Text as Text (pack)
import Data.Monoid


type WebSiteM = WriterT WebSite

instance Monoid WebSite where
  mempty = mzeroWebSite
  mappend = wplus

modifyL :: Monad m => Lens WebSite a -> (a -> a) -> WebSiteM m a -> WebSiteM m a
modifyL lens f = censor (modL lens f)

tellServerPart :: Monad m => ServerPartT IO Response -> WebSiteM m ()
tellServerPart sp = tell $ mempty { serverpart = sp }

tellHead :: Monad m => [WM_Header] -> WebSiteM m ()
tellHead xs = tell $ mempty { headers = xs }

tellBody :: Monad m => [WM_Body] -> WebSiteM m ()
tellBody xs = tell $ mempty { bodies = xs }
  
wimport :: Monad m => WebSite -> a -> WebSiteM m a
wimport s bindings = do
  tellServerPart (serverpart s)
  tellHead (headers s)
  tellBody (bodies s)
  return bindings


mkWebSiteM :: Monad m => WebSite -> WebSiteM m ()
mkWebSiteM ws = tell ws

mzeroWebSite :: WebSite
mzeroWebSite = WebSite { serverpart = mzero
                       , baseURL = []
                       , headers = []
                       , bodies = []
                       , manifest = []
                       }

runWebSiteM  = runWriterT

-- This is probably mapWriterT composed with runWriterT or something.
compileWebSiteM :: Monad m => WebSiteM m a -> m (a, WebSite)
compileWebSiteM m = do
  (a,ws) <- runWebSiteM m
  return (a, f ws)
  where f :: WebSite -> WebSite
        f ws = ws { serverpart = mkTemplatePart ws `mplus` serverpart ws}

mkTemplatePart :: WebSite -> ServerPartT IO Response
mkTemplatePart ws = rootHandler (templateMarkup ws)

-- This is the sole html page pulled from the server, it includes all
-- the javascript, css, initialization code, etc. as well as a DOM
-- element which serves as the application root.
templateMarkup ws = htmlTemplate' title (hs ws) (bs ws)
  where title = Text.pack "Need a title"
        hs = map toMarkup . headers
        bs = map toMarkup . bodies

defaultHandler :: Markup -> ServerPartT IO Response
defaultHandler m = nullDir >> ok (toResponse m)

indexDotHtml :: Markup -> ServerPartT IO Response
indexDotHtml = htmlHandler "index.html"

rootHandler :: Markup -> ServerPartT IO Response
rootHandler m = msum [ defaultHandler m
                     , indexDotHtml m
                     ]
htmlHandler :: FilePath -> Markup -> ServerPartT IO Response
htmlHandler fp m = dirs fp $ ok (toResponse m)
