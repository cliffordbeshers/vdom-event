{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module WebModuleM where

import Control.Applicative
import Control.Monad as Monad
import Control.Monad.Trans.State
import Markable
import WebModule
import Happstack.Server as Happstack (ServerPartT, Response, nullDir, ok, toResponse, dirs)
import Text.Blaze.Html5 (Markup, toMarkup)
import Data.Lens.Strict
import Template
import Data.Text as Text (pack)


newtype WebSiteM m a = WebSiteM { unWebSiteM :: StateT WebSite m a }

instance Functor m => Functor (WebSiteM m) where
  fmap f = WebSiteM . fmap f . unWebSiteM

instance (Applicative m, Monad m) => Applicative (WebSiteM m) where
  pure = WebSiteM . pure
  WebSiteM f <*> WebSiteM a = WebSiteM $ f <*> a

instance Monad m => Monad (WebSiteM m) where
    return = WebSiteM . return
    m >>= k = WebSiteM (unWebSiteM m  >>= unWebSiteM . k )

-- modifyL :: Monad m => Lens s a -> (a -> a) -> WebSiteM m WebSite
modifyL lens f = WebSiteM $ modify (modL lens f)

tellServerPart :: Monad m => ServerPartT IO Response -> WebSiteM m ()
tellServerPart sp = modifyL serverpartLens (`mplus` sp)

tellHead :: Monad m => [WM_Header] -> WebSiteM m ()
tellHead xs = modifyL headersLens (++ xs)

tellBody :: Monad m => [WM_Body] -> WebSiteM m ()
tellBody xs = modifyL bodiesLens (++ xs)
  
wimport :: Monad m => WebSite -> a -> WebSiteM m a
wimport s bindings = do
  tellServerPart (serverpart s)
  tellHead (headers s)
  tellBody (bodies s)
  return bindings


mkWebSiteM :: Monad m => WebSite -> WebSiteM m ()
mkWebSiteM ws = WebSiteM (put ws)

mzeroWebSite :: WebSite
mzeroWebSite = WebSite { serverpart = mzero
                       , baseURL = []
                       , headers = []
                       , bodies = []
                       , manifest = []
                       }

runWebSiteM :: Monad m => WebSiteM m a -> m (a, WebSite)
runWebSiteM m = runStateT (unWebSiteM m) mzeroWebSite

compileWebSiteM :: Monad m => WebSiteM m a -> WebSiteM m ()
compileWebSiteM m = WebSiteM $ modify f
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


evalWebSiteM :: Monad m => WebSiteM m a -> m (a,WebSite)
evalWebSiteM m = do
  (a,ws) <- runStateT (unWebSiteM m) mzeroWebSite
  return (a,ws)


-- instance (Monad m, MonadPlus m) => MonadPlus (WebSiteM m) where
--    mzero = WebSiteM mzero
--    WebSiteM a `mplus` WebSiteM b = do
--      a' :: WebSite <- liftM $ runWebSiteM' a
--      b' <- lift $ runWebSiteM' b
--      mkWebSiteM $ a' `wplus` b'

instance (Applicative m, MonadPlus m) => Alternative (WebSiteM m) where
  empty = WebSiteM mzero
  WebSiteM mzero <|> b = b
  a <|> _ = a
