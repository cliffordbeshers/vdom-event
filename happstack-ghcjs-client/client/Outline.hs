{-# OPTIONS -fwarn-incomplete-patterns #-}
module Outline where

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
import Data.Text.Lazy.Encoding as Text (decodeUtf8)
import Data.Text.Lazy as Text (toStrict)
import Data.Tree
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Foldable as F
import Data.Map -- as Map (Map)


-- I don't want to pass just thunks here, because I want to register
-- encodings, event handlers, etc.
-- Op's are higher level.  Each defines a series of PrimOps,
-- which are low level operations on the DOM which can be run in response
-- to events.  The IdentMap tracks these.

data Op = ToggleShow Ident Ident
        | SetAttr Ident Text Text
        | ClearAttr Ident Text
        | Click Ident [Op] -- Should really be a Free monad lift here.
        deriving Show
-- Primitive operations on the DOM that get triggered by events.
data PrimOp = RemoveHandler (IO ())

type Ident = Text
type IdentSupply = Int

type IdentMap = Map Ident PrimOp

type Binder = [Op]
type Outline a = HtmlT (WriterT Binder (StateT IdentSupply Identity)) a
type OutlineT m a = HtmlT (WriterT Binder (StateT IdentSupply m)) a

new :: Monad m => OutlineT m Ident
new = lift $ do
  i <- get
  put (succ i)
  return . Text.pack . show $ i

bind :: Op -> Outline ()
bind o = lift $ tell [o]

byId :: Text -> Text
byId = (Text.pack "#" <>)


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

renderOutline :: Outline a -> (Text, Binder)
renderOutline o =  let (html,ops,_,_) = runOutline o in (render html, ops)
  where render = Text.toStrict . Text.decodeUtf8

renderOutlineT :: (Functor m, Monad m) => OutlineT m a -> m (Text, Binder)
renderOutlineT o =  do
  (html,ops,_,_) <- runOutlineT o
  return (render html, ops)
    where render = Text.toStrict . Text.decodeUtf8


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
  i <- new
  let h = div_ [id_ i] (toHtml "Hello")
  j <- new
  let g = div_ [id_ j] (toHtml "Goodbye")
  bind (ToggleShow i j)
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

identify' :: Monad m => (T.Traversable t, With a) => t a -> OutlineT m (t (Ident, a))
identify' = T.mapM (\a -> new >>= (\i -> return (i,a `with` [id_ i])))

-- What's next?
-- Make some actually widgets, e.g. open close button for another widget
-- where hovering on either highlights the other.

outline :: (ToHtml a) => Forest a -> Outline (Forest (Ident, a))
outline ts = ul_ (T.mapM outlineTree ts) >> T.mapM identify ts
  where outlineTree (Node a []) = li_ $ toHtml a
        outlineTree (Node a ts) = li_ $ do
          toHtml a
          ul_ $ F.mapM_ outlineTree ts
