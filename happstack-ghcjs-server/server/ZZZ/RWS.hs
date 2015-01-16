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

putMarkup :: MonadState WebSite m => Lens WebSite Markup -> Markup -> m ()
putMarkup lens markup = modify (modL lens (>> markup))

putHead :: MonadState WebSite m => Markup -> m ()
putHead = putMarkup headMarkupLens
  
putBody :: MonadState WebSite m => Markup -> m ()
putBody = putMarkup bodyMarkupLens
