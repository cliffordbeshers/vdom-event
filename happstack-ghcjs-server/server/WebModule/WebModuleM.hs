{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module WebModule.WebModuleM where

import Control.Monad as Monad (MonadPlus(mplus, mzero), msum)
import Control.Monad.Trans.Writer (censor, tell, WriterT(runWriterT))
import Data.Lens.Strict (Lens, modL)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Text as Text (pack)
import Happstack.Server as Happstack (dirs, nullDir, ok, Response, ServerPartT, ToMessage(..))
import Text.Blaze.Html5 (ToMarkup(toMarkup))
import WebModule.Markable (WM_Body, WM_Header)
import WebModule.Template (htmlTemplate')
import WebModule.WebModule (WebSite(..), wplus)

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
  tell s
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

defaultHandler :: ToMessage a => a -> ServerPartT IO Response
defaultHandler m = nullDir >> ok (toResponse m)

indexDotHtml :: ToMessage a => a -> ServerPartT IO Response
indexDotHtml = htmlHandler "index.html"

rootHandler :: ToMessage a => a -> ServerPartT IO Response
rootHandler m = msum [ defaultHandler m
                     , indexDotHtml m
                     ]
htmlHandler :: ToMessage a => FilePath -> a -> ServerPartT IO Response
htmlHandler fp m = dirs fp $ ok (toResponse m)
