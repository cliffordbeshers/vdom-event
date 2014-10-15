{-# LANGUAGE DeriveGeneric #-}
module Sortable (Operation(..), Error(..), update) where

import GHC.Generics
import Data.List (sortBy)
import GHCJS.JQuery
-- jquery sortable.
-- construct with a list of keys
-- handle a permuted list of those keys
-- handle a (Move i j) event, defined as: remove position i, 0 \le i \lt length, reinsert at position j, 0 \le j \lt length.


data Operation = Move Int Int | Permutation [Int] deriving (Eq, Show, Generic)

data Error = ErrorMoveOutOfRange | ErrorPermutation deriving (Eq, Show, Generic)



update :: Operation -> [key] -> Either Error [key]
update op ks =
  case op of
    Move i j -> 
      let l = length ks in
      if i < 0 || i >= l || j < 0 || j >= l then
        Left ErrorMoveOutOfRange
      else let x = ks !! i in
      Right (insertAt x j (deleteAt i ks))
    Permutation ps -> 
      if (length ps /= length ks) then
        Left ErrorPermutation
      else
        Right (permute ps ks)
                    
permute :: [Int] -> [a] -> [a]
permute ps ks = snd . unzip . (sortBy (\(a,_) (b,_) -> compare a b)) $ zip ps ks

insertAt :: a -> Int -> [a] -> [a]
insertAt x j ys =
  let (l,r) = splitAt j ys
  in l ++ [x] ++ r

deleteAt :: Int -> [a] -> [a]
deleteAt i ys =
  let (l,r) = splitAt i ys
  in l ++ tail r
