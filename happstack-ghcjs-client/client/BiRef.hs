import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Bimap
import System.IO.Unsafe
import ControlMonadSupplyExcept
import Lucid


type Ident = Int
data Config = Config

data Widget = Widget { ident :: Ident } deriving Show

data J = Op1 Widget | Op2 Widget Widget


-- Global Monad
type GM m = ReaderT Config (SupplyT Ident (HtmlT (WriterT [J] m)))

gm :: Monad m => GM m a -> m a
gm m = runGM m Config

runGM :: Monad m => GM m a -> Config -> m (a, [J])
runGM m c = runWriterT (evalHtmlT (evalSupplyT (runReaderT m c) [0..]))


a :: Monad m => GM m Widget
a = do
  i <- supply
  return $ Widget { ident = i }

b :: Monad m => GM m Widget
b = do
  i <- supply
  return $ Widget { ident = i }

type DOM = [Widget]

type Ref = (Ident, Ident)
ref = (,)
-- biRef = Widget -> Widget -> GM -- _biRef = undefined

biref a b = [(a,b), (b,a)]
birefW a b = [(ident a,ident b), (ident b,ident a)]

dom :: Monad m => GM m [Ref]
dom = do
  a' <- a
  b' <- b
  return $ birefW a' b'

