module Bootstrap.Forest where

import Control.Applicative
import Data.Sequence

newtype Forest a = Forest { forest :: Seq (Tree a) }

data Tree a = Node a (Forest a) | Leaf a

instance Applicative (Forest v)