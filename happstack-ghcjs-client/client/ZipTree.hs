{-# LANGUAGE ScopedTypeVariables #-}
module ZipTree where

import Prelude
import Control.Applicative ((<$>), Applicative(..), ZipList(ZipList, getZipList))
import Data.List (genericTake)
import Data.Monoid (Monoid(..))
import Data.Tree (drawTree, Forest, Tree(Node))

zipTree_0 :: Tree a -> Tree b -> Tree (a,b)
zipTree_0 (Node a as) (Node b bs) = Node (a,b) (zipForest_0 as bs)

zipForest_0 :: Forest a -> Forest b -> Forest (a,b)
zipForest_0 as bs = map (uncurry zipTree_0) $ zip as bs

zipForest_1 :: Forest a -> Forest b -> Forest (a,b)
zipForest_1 as bs = getZipList $ zipTree_0 <$> ZipList as <*> ZipList bs


z = zipForest_0

-- tests = z [Node "div" []]

-- maybeZ :: (a -> b -> c) -> Tree (Maybe a) -> Tree b -> Tree c
-- maybeZ f tma tb = undefined

-- infiniteForest :: Forest (Maybe a)
infiniteForest :: a -> Forest a
infiniteForest a = repeat (infiniteTree a)
infiniteTree :: a -> Tree a
infiniteTree a = Node a (infiniteForest a)
        
        
takeForest :: (Integral w, Integral d) => w -> d -> Forest a -> Forest a
takeForest _ _ [] = []
takeForest w _ _  | w <= 0 = []
takeForest _ d _  | d <= 0 = []
takeForest width depth ts = map (takeTree width (depth-1)) (genericTake width ts)

takeTree :: (Integral w, Integral d) => w -> d -> Tree a -> Tree a
takeTree w d (Node a us) = Node a (takeForest w d us)

ts = map (\n -> Node (show n) []) ns
  where ns :: [Int]
        ns = [0..20]
        
        
extendForest :: Forest a -> Forest (Maybe a)
extendForest ts = map extendTree ts ++ (infiniteForest Nothing)
extendTree :: Tree a -> Tree (Maybe a)
extendTree  (Node a ts) = Node (Just a) (extendForest ts)

extendForestMonoid :: Monoid b => (a -> b) -> Forest a -> Forest b
extendForestMonoid f ts = map (extendTreeMonoid f) ts ++ (infiniteForest mempty)
extendTreeMonoid :: Monoid b => (a -> b) -> Tree a -> Tree b
extendTreeMonoid f (Node a ts) = Node (f a) (extendForestMonoid f ts)


pruneBy :: Forest b -> Forest a -> Forest a
pruneBy [] _ = []
pruneBy (p:ps) (f:fs) = (pruneTreeBy p f) : (pruneBy ps fs)
  where pruneTreeBy :: Tree b -> Tree a -> Tree a
        pruneTreeBy (Node p ps) (Node t ts) = Node t (pruneBy ps ts)


applyF :: Forest (a -> b) -> Forest a -> Forest b
applyF ff fa = zipWith applyT ff fa

applyT :: Tree (a -> b) -> Tree a -> Tree b        
applyT (Node f fs) (Node a as) = Node (f a) (applyF fs as)

newtype ZipForest a = ZipForest { unZipForest :: Forest a }

instance Functor ZipForest where
  fmap f = ZipForest . fmap (fmap f) . unZipForest

instance Applicative ZipForest where
  pure = ZipForest . infiniteForest
  ZipForest f <*> ZipForest a = ZipForest $ applyF f a

test = putStrLn $ drawTree . fmap show $ applyT (Node (*2) (repeat (Node (*5) []))) (Node 20 [Node 30 [], Node 50 []])



listToForest :: [a] -> Forest a
listToForest [] = []
listToForest (x:xs) = [Node x (listToForest xs)]

-- -- ns :: (Mona => [HtmlT m () -> HtmlT m ()]
-- ns :: Term arg result => Forest (arg -> result)
-- ns = listToForest [ div_, ul_, li_ ]

-- -- cs :: Term arg result => [arg -> result]
-- cs :: Forest [Attribute]
-- cs = listToForest [ [ class_ "hello"], [ class_ "hello"], [ class_ "hello"] ]


-- withF :: With a => Forest (a -> [Attribute] -> a)
-- withF = infiniteForest with

-- helloF :: Forest (Html ())
-- helloF = infiniteForest (toHtml "hello")

-- -- test :: Forest (Html ())
-- test = ns <*> helloF
-- -- data HTree n l = HForest a [HTree

-- jis = [Node (Just 1) [Node Nothing []], Node (Just 3) [Node Nothing []]]