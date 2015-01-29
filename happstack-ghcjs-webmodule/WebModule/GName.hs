{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- This should probably be in a utility module
module WebModule.GName (gname, 
                        Typeable.Proxy(..)) where

import Data.Typeable as Typeable (Proxy(..))
import GHC.Generics

-- |gname is a generic function that calls GHC.Generics.Datatype(datatypeName).
-- gname improves on datatypeName in two ways: requires only a suitably typed Proxy value as input;
-- it is parameterized by Datatype, Generic and Rep, allowing for a polymorphic version.
-- Example:
--  data Foo = Foo deriving Generic
--  s = gname (Proxy :: Proxy Foo)

gname :: forall (t :: * -> (* -> *) -> * -> *) d f a. (Datatype d, Generic a, Rep a ~ t d f) => Proxy a -> String
gname _ = datatypeName (undefined :: Rep a x)

-- DSF's solution to my original request for a type sig, added the forall clause which fixed the kind.
--gname :: forall (t :: * -> (* -> *) -> * -> *) d f a. (Datatype d, Generic a, Rep a ~ t d f) => a -> String
--gname = datatypeName . from 

-- Make sure the given example works.
data Foo = Foo deriving Generic

_test = gname (Proxy :: Proxy Foo)

