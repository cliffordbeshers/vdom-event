
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

type WebSiteM = Writer WebSite

runWebSite :: WebSiteM a -> (a, WebSite)
runWebSite wsm = runWriter wsm

runGui :: (WebView -> IO ()) -> IO ()
runGui = ($ WebView)

compileGUI :: WebSiteM GUI -> GUI
compileGUI wsm = let (a,_) = runWebSite wsm in a

main = do
  let gui = compileGUI ws
  runGui gui

ws :: WebSiteM GUI
ws = do
  tell WebSite
  return (\x -> print x)
  
