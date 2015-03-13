import Lucid
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text as Text (Text, pack)


data Op = Op deriving Show
type Ident = Int
type IdentSupply = Int

type Binder = [(Op,Ident,Ident)]
type Outline a = HtmlT (WriterT Binder (StateT IdentSupply Identity)) a
type OutlineT m a = HtmlT (WriterT Binder (StateT IdentSupply m)) a

new :: Outline Int
new = lift $ do
  i <- get
  put (succ i)
  return i

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
  i <- fmap tshow new
  let h = div_ [id_ i] (toHtml "Hello")
  j <- fmap tshow new
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
