{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Generics

class Datatype' a where
  datatypeName' :: a -> String
  moduleName' :: a -> [Char]
  isNewtype' :: a -> Bool

  default datatypeName' :: (Generic a, Datatype (Rep a)) => a -> String
  datatypeName' = datatypeName . from

  default moduleName' :: (Generic a, Datatype (Rep a)) => a -> String
  moduleName' = moduleName . from

  default isNewtype' :: (Generic a, Datatype (Rep a)) => a -> String
  isNewtype' = isNewtype . from
