{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader

data WebView = WebView deriving Show
data WebSite = WebSite deriving Show

instance Monoid WebSite where
  mempty = WebSite
  mappend a b = WebSite

type GUI = WebView -> IO ()

newtype WebSiteM r m a = WebSiteM { unWebSiteM :: ReaderT r (WriterT WebSite m) a } deriving (Functor, Applicative, Monad)

instance MonadTrans (WebSiteM r) where
  lift = WebSiteM . lift . lift

instance (MonadIO m) => MonadIO (WebSiteM r m) where
  liftIO = lift . liftIO

data Message = Message String deriving Show

runWebSite :: Monad m => WebSiteM r m a -> r -> m (a, WebSite)
runWebSite wsm r = runWriterT (runReaderT (unWebSiteM wsm) r)

runGui :: (WebView -> IO ()) -> IO ()
runGui = ($ WebView)

compileGUI :: Monad m => WebSiteM r m GUI -> r -> m GUI
compileGUI wsm r = do
  (a,_ws) <- runWebSite wsm r
  return a

main = do
  gui <- compileGUI ws (Message "Hello, world!")
  runGui gui

ws :: (Show r, Monad m) => WebSiteM r m GUI
ws = do
  WebSiteM $ lift $ tell WebSite
  -- message <- lift ask
  return (\x -> print x) --  >> print message)
  
