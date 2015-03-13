import Lucid
import Data.Tree
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text as Text

data Foo = Foo deriving Show
data Bar = Bar deriving Show

fooToBar :: Foo -> Bar
fooToBar Foo = Bar

-- x :: Monad m => HtmlT m Foo
-- x = (return Foo) >>= div_ []

divx :: Monad m => HtmlT m Foo
divx = lift $ return Foo

-- divy :: Monad m => HtmlT m a
-- divy = (lift $ return Foo) >>= (div_ [])

-- xs :: Monad m => HtmlT m String
-- xs  = fmap show x

-- divb :: Monad m => HtmlT m Bar
-- divb = fmap fooToBar x


t :: Tree Int
t = Node 1 [Node 2 [], Node 3 []]

instance ToHtml Int where
  toHtml = toHtml . show
  toHtmlRaw = toHtmlRaw . show

instance ToHtml Foo where
  toHtml = toHtml . show
  toHtmlRaw = toHtmlRaw . show

-- hi :: Monad m => HtmlT m Int
-- hi = div_ $ do
--    (lift $ return (1 :: Int)) >>= span_
--    div_ $ do
--      lift $ return 1
--      lift $ return 2
--    spans [lift $ return 1, lift $ return 2]


spans :: Monad m => [HtmlT m a] -> HtmlT m a
spans = Prelude.foldr1 (>>)


f :: (ToHtml a, Monad m) => Tree a -> HtmlT m ()
f (Node i []) = span_ $ toHtml i
f (Node i ts) =
  div_ $ do
    h2_ (toHtml i)
    mapM_ f ts



rdr = runReader (renderTextT (html_ (body_ (do name <- lift ask
                                               p_ [class_ (Text.pack "name")] (toHtml name)))))
      ("Chris" :: String)
