{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module DataTypeName where
import GHC.Generics

data Foo = Foo deriving (Show, Generic)

fooname :: String
fooname = datatypeName . from $ Foo

-- What type signature will make this work?
--gname :: (Datatype d, Generic a, Rep a ~ t d f) => a -> [Char]
--gname = datatypeName . from



