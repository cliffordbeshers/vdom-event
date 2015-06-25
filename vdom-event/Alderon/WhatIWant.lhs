

> newtype Forest a = Forest { unForest :: [Tree a] }
> data Tree a = Node a (Forest a)

> tree :: a -> Forest a -> Tree a
> tree = Node

> forest :: [Tree a] -> Forest a
> forest = Forest

Ease of construction of subtrees.

> type Html = String

> skeleton :: Forest Html -> Tree Html
> skeleton = div_ . ul_ . many li_

Forest Html is many Tree Html.

> many :: ([Tree a] -> Tree a) -> [Tree a] -> Forest a
> many = forest . map

> deforest :: Forest a -> [a]
> deforest = unForest

> div_ = tree "div"
> ul_ = tree "ul"
> li_ = tree "li"
