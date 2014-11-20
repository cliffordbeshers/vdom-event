{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Monoid
-- import Control.Monad.Trans
-- import Control.Monad.RWS.Lazy
import Control.Monad.State
-- import Control.Monad.Trans.State
import qualified GHCJSStub.JQuery as JQuery (on, Event(..), EventType(..), HandlerSettings, JQuery)

-- foo :: RWS Int String (Int,Int) Float
-- foo = do
--   i <- ask
--   (j,k) <- get
--   tell $ show (i, (j,k))
--   put (i+j, i+k)
--   return ( fromIntegral (i * j * k))


-- runfoo = runRWS foo  13 (5,7)


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

instance (Monad m, MonadPlus m) => MonadPlus (Bar m) where
    mzero = Bar mzero
    Bar a `mplus` Bar b = Bar $ a `mplus` b

instance (Applicative m, MonadPlus m) => Alternative (Bar m) where
  empty = Bar mzero
  Bar mzero <|> b = b
  a <|> _ = a


data Head = Head String deriving (Eq, Show)
data Body = Body String deriving (Eq, Show)
data S = S { heads :: [Head], bodies :: [Body]} deriving Show

splus :: S -> S -> S
splus a b = S { heads = heads a <> heads b, bodies = bodies a <> bodies b }

instance Monoid S where
  mempty = S [] []
  mappend = splus

newtype WrapS m a = WrapS { unWrapS :: StateT S m a }

instance Functor m => Functor (WrapS m) where
  fmap f = WrapS . fmap f . unWrapS

instance (Applicative m,  Monad m) => Applicative (WrapS m) where
  pure = WrapS . pure
  WrapS f <*> WrapS a = WrapS $ f <*> a

instance Monad m => Monad (WrapS m) where
  return = WrapS . return
  m >>= k = WrapS (unWrapS m  >>= unWrapS . k )

instance (Applicative m, MonadPlus m) => Alternative (WrapS m) where
  empty = WrapS mzero
  WrapS mzero <|> b = b
  a <|> _ = a

--instance MonadTrans WrapS where
--   lift m = WrapS m -- $ do
--     a <- m
--     return a
-- instance MonadTrans (StateT s) where
--     lift m = StateT $ \s -> do
--         a <- m
--         return (a, s)

tellHead :: Monad m => [Head] -> WrapS m ()
tellHead hs = WrapS $ modify (f hs)
  where f xs (S hs bs) = S (hs ++ xs) bs

tellBody :: Monad m => [Body] -> WrapS m ()
tellBody hs = WrapS $ modify (f hs)
  where f xs (S hs bs) = S hs (bs ++ xs)
  
wimport :: Monad m => S -> a -> WrapS m a
wimport s bindings = do
  tellHead (heads s)
  tellBody (bodies s)
  return bindings

jquery :: Monad m => WrapS m JQueryBindings
jquery = wimport (S [Head "jquery import"] [Body "jquery initialization"]) jQueryBindings

data JQueryBindings = JQueryBindings { on :: (JQuery.Event -> IO ()) -> JQuery.EventType -> JQuery.HandlerSettings -> JQuery.JQuery -> IO (IO ()) }

jQueryBindings :: JQueryBindings
jQueryBindings = JQueryBindings { on = JQuery.on }

data BootStrap = BootStrap

bootstrap :: Monad m => WrapS m BootStrap
bootstrap = do
  WrapS $ modify $ splus (S [Head "bootstrap import"] [Body "bootstrap initialization"])
  return BootStrap

website :: Monad m => WrapS m String
website = do
  jq <- jquery
  bs <- bootstrap
  return "Hello, World"
  
website2 :: Monad m => WrapS m String
website2 = do
  jq <- jquery
  bs <- bootstrap
  return "Goodbye, World"

x :: Monad m => WrapS m String
x = website >> website2



-- instance (Monad m, MonadPlus m) => MonadPlus (WrapS m) where
--   mzero = WrapS mzero
--   WrapS a `mplus` WrapS b = do
--     as <- WrapS $ execStateT a mempty
--     bs <- WrapS $ execStateT b mempty
--     WrapS $ (a `mplus` b) >> put (as <> bs)

-- newtype Zeb m a = Zeb { unZeb :: RWST () () S m a }

-- instance Functor m => Functor (Zeb m) where
--   fmap f = Zeb . fmap f . unZeb

-- instance (Applicative m,  Monad m) => Applicative (Zeb m) where
--   pure = Zeb . pure
--   Zeb f <*> Zeb a = Zeb $ f <*> a
  
-- instance Monad m => Monad (Zeb m) where
--     return = Zeb . return
--     m >>= k = Zeb (unZeb m  >>= unZeb . k )

-- instance (Monad m, MonadPlus m) => MonadPlus (Zeb m) where
--     mzero = Zeb mzero
--     Zeb a `mplus` Zeb b = Zeb $ a `mplus` b

-- instance (Applicative m, MonadPlus m) => Alternative (Zeb m) where
--   empty = Zeb mzero
--   Zeb mzero <|> b = b
--   a <|> _ = a

-- runZeb :: Monad m => Zeb m a -> m S
-- runZeb z = do
--   (_, s, _) <- runRWST (unZeb z) () mempty
--   return s

-- mkZeb :: Monad m => S -> Zeb m ()
-- mkZeb = Zeb . put

