{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module WebModuleM where

import Control.Applicative
import Control.Monad as Monad
import Control.Monad.Trans.State
import WebModule
import Happstack.Server as Happstack (ServerPartT, Response)
import Text.Blaze.Html5 (Markup)
import Data.Lens.Strict


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

putServerPart :: Monad m => ServerPartT IO Response -> WebSiteM m ()
putServerPart sp = undefined -- modifyL serverpartLens (`mplus` sp)

-- putHead :: Monad m => Markup -> WebSiteM m ()
-- putHead markup = modifyL headersLens (>> markup)
  
-- putBody :: Monad m => Markup -> WebSiteM m ()
-- putBody markup = modifyL bodiesLens (>> markup)

-- wimport :: MonadState WebSite m => a ->  m a
-- wimport a = do
--   ws <- get
--   putHead (headMarkup ws)
--   putBody (bodyMarkup ws)
--   return a
  
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
