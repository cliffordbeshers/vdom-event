{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Typeable (Proxy(..))
import GHC.Generics

data Foo = Foo deriving Generic

--gname :: forall (t :: * -> (* -> *) -> * -> *) d f a. (Datatype d, Generic a, Rep a ~ t d f) => a -> String
--gname = datatypeName . from 

gname' :: forall (t :: * -> (* -> *) -> * -> *) d f a. (Datatype d, Generic a, Rep a ~ t d f) => Proxy a -> String
gname' _ = datatypeName (undefined :: Rep a x)


--s = gname (Proxy :: Proxy Foo)

