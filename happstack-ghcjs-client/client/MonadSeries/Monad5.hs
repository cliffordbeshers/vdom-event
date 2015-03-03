{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

import Data.Monoid
import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.Writer
import "mtl" Control.Monad.Writer.Class
import "mtl" Control.Monad.State
import "mtl" Control.Monad.State.Class

data WebView = WebView deriving Show
data WebSite = WebSite deriving Show

instance Monoid WebSite where
  mempty = WebSite
  mappend a b = WebSite

type GUI = WebView -> IO ()

newtype WebSiteM r m a = WebSiteM { unWebSiteM :: ReaderT r (WriterT WebSite m) a } deriving (Functor, Applicative, Monad)

instance Monad m => MonadReader r (WebSiteM r m) where
  ask = WebSiteM ask
  local f = WebSiteM . local f . unWebSiteM

instance Monad m => MonadWriter WebSite (WebSiteM r m) where
  tell = WebSiteM . lift . tell
  listen m = WebSiteM . ReaderT $ \r -> WriterT $ do
    ~(a,w) <- runWebSiteM m r
    return  ((a, w), w)
  pass m = WebSiteM . ReaderT $ \r -> WriterT $ do
    ((a,f), w) <- runWebSiteM m r
    return (a, f w)

instance MonadTrans (WebSiteM r) where
  lift = WebSiteM . lift . lift

instance (MonadIO m) => MonadIO (WebSiteM r m) where
  liftIO = lift . liftIO

data Message = Message String deriving Show

runWebSiteM :: Monad m => WebSiteM r m a -> r -> m (a, WebSite)
runWebSiteM wsm r = runWriterT (runReaderT (unWebSiteM wsm) r)

runGui :: (WebView -> IO ()) -> IO ()
runGui = ($ WebView)

compileGUI :: Monad m => WebSiteM r m GUI -> r -> m GUI
compileGUI wsm r = do
  (a,_ws) <- runWebSiteM wsm r
  return a

main = do
  gui <- compileGUI ws (Message "Hello, world!")
  runGui gui

ws :: (Show r, Monad m) => WebSiteM r m GUI
ws = do
  message <- ask
  tell WebSite
  return (\x -> print x) --  >> print message)
  
