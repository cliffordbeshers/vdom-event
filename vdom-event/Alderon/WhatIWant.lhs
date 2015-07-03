> {-# LANGUAGE DeriveFunctor #-}

> import Data.Monoid
> import Control.Applicative
> import Alderon.GenericZip

> newtype Forest a = Forest { deForest :: [Tree a] } deriving (Show)
> data Tree a = Node a (Forest a) deriving (Show)

> instance Monoid (Forest a) where
>   mempty = Forest []
>   mappend (Forest xs) (Forest ys) = Forest (xs `mappend` ys)

> tree :: a -> Forest a -> Tree a
> tree = Node

> leaf :: a -> Tree a
> leaf l = tree l mempty

> forest :: Tree a -> Forest a
> forest = Forest . (:[])

> -- many :: ([Tree a] -> Tree a) -> [Tree a] -> Forest a
> -- many = forest . fmap

Ease of construction of subtrees.

> data DOMNode = DOMNode { tag_ :: Text } | DOMText Text deriving (Show)
> type Html = DOMNode
> type Text = String

> defaultDOMNode = DOMNode ""

> tag :: Text -> DOMNode
> tag t = defaultDOMNode { tag_ = t }

> text :: Text -> Forest Html
> text = forest . leaf . DOMText

> skeleton :: Forest Html -> Forest Html
> skeleton = div_ . ul_ . li_

Forest Html is many Tree Html.

> div_ , ul_, li_ :: Forest Html -> Forest Html
> div_ = forest . tree (tag "div")
> ul_ = forest . tree (tag "ul")
> li_ = forest . tree (tag "li")


Three types, Forest a, Tree a, [Tree a].  We should never see [Tree
a].  And Forest a should probably be the default type for composition.


> instance Functor Tree where
>   fmap f (Node x ts) = Node (f x) (fmap f ts)

> instance Functor Forest where
>   fmap f (Forest ts) = Forest (fmap (fmap f) ts)

> instance Applicative Tree where
>     pure x = Node x (Forest [])
>     Node f (Forest ffs) <*> tx@(Node x (Forest fxs)) =
>        Node (f x) (Forest $  (fmap (f <$>) fxs ++ fmap (<*> tx) ffs))
> --    Node f ffs <*> tx@(Node x fxs) =
> --        Node (f x) (fmap (fmap (f <$>)) fxs <> fmap (fmap (<*> tx)) ffs)


-- > instance Applicative Forest where
-- >   pure = Forest . (:[]) . pure
-- >   Forest fs <*> Forest xs = Forest (fs <*> xs)

-- > af :: Applicative f => f (Tree (a ->  b)) -> f (Tree a) -> f (Tree b)
-- > af `af` ax = Forest [t]
-- >   where t :: Tree b
-- >         t = Node (f x) subs
-- >         Node f (Forest fs) = af
-- >         Node x (Forest xs) = ax
-- >         subs :: Forest b
-- >         subs = Forest (fmap (fmap f) xs ++ 
--      Node f (Forest ffs) <*> tx@(Node x (Forest fxs)) =
-- >        Node (f x) (Forest $  (fmap (f <$>) fxs ++ fmap (<*> tx) ffs))



instance Monad Forest where
  return = forest . leaf
  (Forest ts) >>= f =  ts
      where _q :: (a -> Forest b) -> [Tree a] -> Forest b
            _q = undefined

instance Monad Tree where
  return = leaf
  (Node a fs) >>= f = f a



