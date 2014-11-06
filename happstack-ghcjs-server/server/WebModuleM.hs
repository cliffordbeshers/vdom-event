{-# LANGUAGE FlexibleContexts #-}
module WebModuleM where

import Control.Monad.RWS.Lazy
import WebModule
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

modifyL :: MonadState s m => Lens s a -> (a -> a) -> m ()
modifyL lens f = modify (modL lens f)

putMarkup :: MonadState WebSite m => Lens WebSite Markup -> Markup -> m ()
putMarkup lens markup = modifyL lens (>> markup)

putHead :: MonadState WebSite m => Markup -> m ()
putHead = putMarkup headMarkupLens
  
putBody :: MonadState WebSite m => Markup -> m ()
putBody = putMarkup bodyMarkupLens

wimport :: MonadState WebSite m => a ->  m a
wimport a = do
  ws <- get
  putHead (headMarkup ws)
  putBody (bodyMarkup ws)
  return a