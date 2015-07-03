module Alderon.GeneralizedZipWith where

import Prelude hiding (sequence)
 
import Data.Sequence
import Data.Foldable
import Data.Traversable
import Control.Applicative
 
 
data Supply s v = Supply { unSupply :: [s] -> ([s],v) }
 
instance Functor (Supply s) where 
  fmap f av = Supply (\l -> let (l',v) = unSupply av l in (l',f v))
 
instance Applicative (Supply s) where
  pure v    = Supply (\l -> (l,v))
  af <*> av = Supply (\l -> let (l',f)  = unSupply af l
                                (l'',v) = unSupply av l'
                            in (l'',f v))
 
runSupply :: (Supply s v) -> [s] -> v
runSupply av l = snd $ unSupply av l
 
supply :: Supply s s
supply = Supply (\(x:xs) -> (xs,x))
 
zipTF :: (Traversable t, Foldable f) => t a -> f b -> t (a,b)
zipTF t f = runSupply (traverse (\a -> (,) a <$> supply) t) (toList f)
 
zipWithTF :: (Traversable t,Foldable f) => (a -> b -> c) -> t a -> f b -> t c
zipWithTF g t f = runSupply  (traverse (\a -> g a <$> supply) t) (toList f)
 
zipWithTFM :: (Traversable t,Foldable f,Monad m) => 
              (a -> b -> m c) -> t a -> f b -> m (t c)
zipWithTFM g t f = sequence (zipWithTF g t f)
 
zipWithTFA :: (Traversable t,Foldable f,Applicative m) => 
              (a -> b -> m c) -> t a -> f b -> m (t c)
zipWithTFA g t f = sequenceA (zipWithTF g t f)
