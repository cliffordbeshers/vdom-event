> {-# LANGUAGE DeriveFunctor #-}

> newtype Forest a = Forest { deForest :: [Tree a] } deriving (Show, Functor)
> data Tree a = Node a (Forest a) deriving (Show, Functor)

> tree :: a -> Forest a -> Tree a
> tree = Node

> forest :: Tree a -> Forest a
> forest = Forest . (:[])

> -- many :: ([Tree a] -> Tree a) -> [Tree a] -> Forest a
> -- many = forest . fmap

Ease of construction of subtrees.

> type Html = String

> skeleton :: Forest Html -> Forest Html
> skeleton = div_ . ul_ . li_

Forest Html is many Tree Html.

> div_ , ul_, li_ :: Forest Html -> Forest Html
> div_ = forest . tree "div"
> ul_ = forest . tree "ul"
> li_ = forest . tree "li"


Three types, Forest a, Tree a, [Tree a].  We should never see [Tree
a].  And Forest a should probably be the default type for composition.




