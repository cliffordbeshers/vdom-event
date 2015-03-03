
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader

data WebView = WebView deriving Show
data WebSite = WebSite deriving Show

instance Monoid WebSite where
  mempty = WebSite
  mappend a b = WebSite

type GUI = WebView -> IO ()

type WebSiteM r m a = ReaderT r (WriterT WebSite m) a

data Message = Message String deriving Show

runWebSite :: Monad m => WebSiteM r m a -> r -> m (a, WebSite)
runWebSite wsm r = runWriterT (runReaderT wsm r)

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
  lift $ tell WebSite
  message <- ask
  return (\x -> print x >> print message)
  
