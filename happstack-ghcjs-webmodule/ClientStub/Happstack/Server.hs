{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
module ClientStub.Happstack.Server where
-- Client side definitions for Happstack.Server

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (MonadPlus(..))

newtype ServerPartT (m :: * -> *) a = ServerPartT { unServerPartT :: m a } deriving Functor

type Response = ()

instance (Applicative m) => Applicative (ServerPartT m) where
  pure = ServerPartT . pure
  ServerPartT f <*> ServerPartT b = ServerPartT (f <*> b)

instance (Applicative m) => Alternative (ServerPartT m) where
  empty = undefined
  _a <|> _b = undefined

instance Monad m => Monad (ServerPartT m) where
  return = ServerPartT . return
  _m >>= _f = undefined

instance MonadPlus m => MonadPlus (ServerPartT m) where
  mzero = ServerPartT mzero
  mplus _ _ = mzero
  
