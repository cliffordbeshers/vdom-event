{-# LANGUAGE FlexibleContexts #-}
module WebModuleM where

import Control.Monad as Monad (mplus)
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.RWS.Lazy
import WebModule
import Happstack.Server as Happstack (ServerPartT, Response)
import Text.Blaze.Html5 (Markup)
import Data.Lens.Strict

foo :: RWS Int String (Int,Int) Float
foo = do
  i <- ask
  (j,k) <- get
  tell $ show (i, (j,k))
  put (i+j, i+k)
  return ( fromIntegral (i * j * k))


runfoo = runRWS foo  13 (5,7)

type WebSiteM = RWST () () WebSite

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
  

mzeroWebSite :: WebSite
mzeroWebSite = WebSite { serverpart = mzero
                       , baseURL = []
                       , headMarkup = return ()
                       , bodyMarkup = return ()
                       , manifest = []
                       }

runWebSiteM :: Monad m => WebSiteM m a -> m (ServerPartT IO Response)
runWebSiteM m = do
  (_, s, _) <- runRWST m () mzeroWebSite
  return $ serverpart s
