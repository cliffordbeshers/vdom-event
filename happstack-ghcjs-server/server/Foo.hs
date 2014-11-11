import Control.Applicative
import Control.Monad.Trans


newtype WebSite a = WebSite { unWebSite :: a }
newtype WebSiteM m a = WebSiteM { unWebSiteM :: m a }

newtype Foo a = Foo { unFoo :: a } deriving Show

instance Functor Foo where
  fmap f (Foo a) = Foo (f a)

instance Applicative Foo where
  pure = Foo
  Foo f <*> Foo a = Foo (f a)
  
instance Monad Foo where
  return = Foo
  a >>= f = f (unFoo a)

newtype Bar m a = Bar { unBar :: m a } deriving (Show)

instance Functor m => Functor (Bar m) where
  fmap f = Bar . fmap f . unBar

instance Applicative m => Applicative (Bar m) where
  pure = Bar . pure
  Bar f <*> Bar a = Bar $ f <*> a
  
instance Monad m => Monad (Bar m) where
    return = Bar . return
    m >>= k = Bar (unBar m  >>= unBar . k )


