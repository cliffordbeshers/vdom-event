
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

type WebSiteM = WriterT WebSite

runWebSite :: Monad m => WebSiteM m a -> m (a, WebSite)
runWebSite wsm = runWriterT wsm

runGui :: (WebView -> IO ()) -> IO ()
runGui = ($ WebView)

compileGUI :: Monad m => WebSiteM m GUI -> m GUI
compileGUI wsm = do
  (a,_ws) <- runWebSite wsm
  return a

main = do
  gui <- compileGUI ws
  runGui gui

ws :: Monad m => WebSiteM m GUI
ws = do
  tell WebSite
  return (\x -> print x)
  
