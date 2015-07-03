module Alderon.TreeZip where

import Data.Tree
import Alderon.GenericZip


t1 :: Int -> Tree Int
t1 n | n < 2 = Node n []
t1 n = Node n [t1 (n-1), t1 (n-1)]

t2 :: Int -> Tree Int
t2 n | n < 2 = Node n []
t2 n = Node n [t2 (n-1), t2 (n-2)]


test = zipTF (t1 1) (fmap show (t1 2))


-- Data.Tree.unfoldTree :: (b -> (a, [b])) -> b -> Tree a

f1 :: Int -> (Int, [Int])
f1 n | n < 2 = (n,[])
f1 n = (n,[n-1,n-1])

f2 :: Int -> (Int, [Int])
f2 n | n < 2 = (n,[])
f2 n = (n,[n-1,n-2])

listify = (:[])
leaf x = Node x []

data Tag = DIV | UL | LI

ulli :: [a] -> Tree (Either Tag a)
ulli = Node (Left UL) . map (Node (Left LI) . listify . leaf . Right)

div_ :: [Tree (Either Tag a)] -> Tree (Either Tag a)
div_ = Node (Left DIV)

exam = zipWithTFM
