import Prelude  hiding (mapM, mapM_)
import Lucid
import Control.Monad.Identity hiding (mapM, mapM_)
import Control.Monad.Writer hiding (mapM, mapM_)
import Control.Monad.State hiding (mapM, mapM_)
import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text as Text (Text, pack)
import Data.Tree
import qualified Data.Traversable as T
import qualified Data.Foldable as F


data Op = Op deriving Show
type Ident = Text
type IdentSupply = Int

type Binder = [(Op,Ident,Ident)]
type Outline a = HtmlT (WriterT Binder (StateT IdentSupply Identity)) a
type OutlineT m a = HtmlT (WriterT Binder (StateT IdentSupply m)) a

new :: Outline Ident
new = lift $ do
  i <- get
  put (succ i)
  return $ tshow i

bind :: Op -> Ident -> Ident -> Outline ()
bind o i j = lift $ tell [(o,i,j)]

runHtmlT' :: Monad m => HtmlT m a -> m (ByteString,a)
runHtmlT' m =
  do (f,a) <- runHtmlT m
     return (Blaze.toLazyByteString (f mempty), a)

runOutline :: Outline a -> (ByteString, Binder, IdentSupply, a)
runOutline = flatten . runIdentity . (flip runStateT 0) . runWriterT . runHtmlT'
  where flatten :: (((ByteString, a), Binder), IdentSupply) -> (ByteString, Binder, IdentSupply, a)
        flatten (((html, a), binder), identSupply) = (html, binder, identSupply, a)

runOutlineT :: (Functor m, Monad m) => OutlineT m a -> m (ByteString, Binder, IdentSupply, a)
runOutlineT = fmap flatten . (flip runStateT 0) . runWriterT . runHtmlT'
  where flatten :: (((ByteString, a), Binder), IdentSupply) -> (ByteString, Binder, IdentSupply, a)
        flatten (((html, a), binder), identSupply) = (html, binder, identSupply, a)


tshow :: Show a => a -> Text
tshow = Text.pack . show

data A = A deriving Show

example :: Outline A
example = do
  i <- new
  let h = div_ [id_ i] (toHtml "Hello")
  j <- new
  let g = div_ [id_ j] (toHtml "Goodbye")
  div_ $ h >> g
  return A

instance ToHtml A where
  toHtml = toHtml . show
  toHtmlRaw = toHtmlRaw . show

div' :: (Monad m, ToHtml a) => a -> HtmlT m a
div' a = do
  div_ (toHtml a)
  return a

example2 :: Outline A
example2 = return A >>= div'

bindExample :: Outline A
bindExample = do
  i <- fmap tshow new
  let h = div_ [id_ i] (toHtml "Hello")
  j <- fmap tshow new
  let g = div_ [id_ j] (toHtml "Goodbye")
  bind Op i j
  div_ $ h >> g
  return A

f :: (ToHtml a) => Tree a -> Outline (Tree a)
f n@(Node i []) = (span_ $ toHtml i) >> return n
f n@(Node i ts) = do
  div_ $ do
    h2_ (toHtml i)
    F.mapM_ f ts
  return n

z :: (ToHtml a) => Tree a -> Outline (Tree (Ident,a))
z (Node n []) = new >>= (\i -> return (Node (i,n) []))
z (Node n ts) = do
  (Node n' _) <- z (Node n [])
  ts' <- T.mapM z ts
  return (Node n' ts')

treeExample :: Outline (Tree (Ident, A))
treeExample = do
  let t = Node A [Node A [], Node A []]
  identify t

forMExample :: Outline (Tree (Ident, A))
forMExample = do
  let t = Node A [Node A [], Node A []]
  T.mapM (\a -> new >>= (\i -> return (i,a))) t 

identify :: T.Traversable t => t a -> Outline (t (Ident, a))
identify = T.mapM (\a -> new >>= (\i -> return (i,a)))
