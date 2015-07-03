import Control.Applicative


data List a = Cons a (List a) | Nil deriving Show

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> xs = fromList [ f x | f <- toList fs, x <- toList xs ]

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x:(toList xs)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

