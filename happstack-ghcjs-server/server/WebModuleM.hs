{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module WebModuleM where

import Control.Applicative
import Control.Monad as Monad (mplus)
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.RWS.Lazy
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

modifyL :: MonadState s m => Lens s a -> (a -> a) -> m ()
modifyL lens f = modify (modL lens f)

putServerPart :: MonadState WebSite m => ServerPartT IO Response -> m ()
putServerPart sp = modifyL serverpartLens (`mplus` sp)

putHead :: MonadState WebSite m => Markup -> m ()
putHead markup = modifyL headMarkupLens (>> markup)
  
putBody :: MonadState WebSite m => Markup -> m ()
putBody markup = modifyL bodyMarkupLens (>> markup)

wimport :: MonadState WebSite m => a ->  m a
wimport a = do
  ws <- get
  putHead (headMarkup ws)
  putBody (bodyMarkup ws)
  return a
  
mkWebSiteM :: Monad m => WebSite -> WebSiteM m ()
mkWebSiteM ws = WebSiteM (put ws)



mzeroWebSite :: WebSite
mzeroWebSite = WebSite { serverpart = mzero
                       , baseURL = []
                       , headMarkup = return ()
                       , bodyMarkup = return ()
                       , manifest = []
                       }

runWebSiteM :: Monad m => WebSiteM m a -> m WebSite
runWebSiteM m = do
  s <- execStateT (unWebSiteM m) mzeroWebSite
  return s


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
