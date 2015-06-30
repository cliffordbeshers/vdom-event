{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Alderon.Html.Internal
    ( -- * Elements
      Node(..)
      -- * Attributes
    , Id
    , Handlers
    , Attributes(..)
    , defaultAttributes
      -- * Html
    , Html
    , HtmlM(..)
    , runHtml
    , parent
    , leaf
    , text_
      -- * Event handlers
    , EventType(..)
    , Event(..)
    , Handler(..)
      -- * Setting attributes
    , Attribute
    , Attributable
    , (!)
    , (!?)
    , (!#)
    , (!.)
    , (!?.)
    , key
      -- * Creating attributes
    , attribute
    , boolean
    , onEvent
    , JSObj
    ) where

import           Control.Applicative
import           Data.DList          as DList
import           Data.Hashable
import           Data.HashMap.Strict as HashMap hiding ((!))
import           Data.Monoid
import           Data.Text           as Text
import           Data.Tree
import           Unsafe.Coerce
import           GHCJS.Types
import           MicroDOM

import           Alderon.JavaScript

infixl 1 !, !?, !#, !., !?.

data Node
    = Element !Text Attributes
    | TextNode !Text

type Id = Text

type Handlers = HashMap EventType (JSObj -> IO ())

data Attributes = Attributes
    { elementId       :: !(Maybe Id)
    , elementKey      :: !(Maybe Int)
    , elementClass    :: ![Text]
    , otherAttributes :: !(HashMap Text Text)
    , handlers        :: !Handlers
    }

defaultAttributes :: Attributes
defaultAttributes = Attributes
    { elementId       = Nothing
    , elementKey      = Nothing
    , elementClass    = []
    , otherAttributes = HashMap.empty
    , handlers        = HashMap.empty
    }

type Html = HtmlM ()

newtype HtmlM a = HtmlM (Attributes -> DList (Tree Node))
    deriving (Monoid)

instance Functor HtmlM where
    fmap _ = unsafeCoerce

instance Applicative HtmlM where
    pure _ = mempty
    (<*>)  = appendHtml

instance Monad HtmlM where
    return _ = mempty
    (>>)     = appendHtml
    m >>= k  = m `appendHtml` k (error "Alder.HtmlM: monadic bind")

appendHtml :: HtmlM a -> HtmlM b -> HtmlM c
appendHtml a b = unsafeCoerce a <> unsafeCoerce b

runHtml :: Html -> Forest Node
runHtml (HtmlM f) = DList.toList (f defaultAttributes)

parent :: Text -> Html -> Html
parent t h = HtmlM $ \a -> DList.singleton (Node (Element t a) (runHtml h))

leaf :: Text -> Html
leaf t = HtmlM $ \a -> DList.singleton (Node (Element t a) [])

text_ :: Text -> Html
text_ t = HtmlM $ \_ -> DList.singleton (Node (TextNode t) [])

addAttribute :: Attribute -> HtmlM a -> HtmlM a
addAttribute (Attribute f) (HtmlM g) = HtmlM (g . f)

data EventType
    = KeyDown | KeyPress | KeyUp
    | Focus | Blur
--    | FocusIn | FocusOut -- These are not widely supported yet.
    | Input | Change
    | Submit
    | MouseDown | MouseUp | Click | DoubleClick | MouseMove | MouseEnter | MouseLeave
    | Drag | DragStart | DragEnd | DragEnter | DragLeave | DragOver | Drop
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Hashable EventType where
    hashWithSalt s = hashWithSalt s . fromEnum

class Event e where
    extractEvent :: JSObj -> IO e

class Handler f where
    fire :: f e -> e -> IO ()

newtype Attribute = Attribute (Attributes -> Attributes)

instance Monoid Attribute where
    mempty = Attribute id
    mappend (Attribute f) (Attribute g) = Attribute (f . g)

class Attributable h where
    (!) :: h -> Attribute -> h

instance Attributable (HtmlM a) where
    (!) = flip addAttribute

instance Attributable h => Attributable (r -> h) where
    f ! a = (! a) . f

(!?) :: Attributable h => h -> (Bool, Attribute) -> h
h !? (p, a) = if p then h ! a else h

(!#) :: Attributable h => h -> Text -> h
h !# v = h ! Attribute addId
  where
    addId a = a { elementId = Just v }

(!.) :: Attributable h => h -> Text -> h
h !. v = h ! Attribute addClass
  where
    addClass a = a { elementClass = v : elementClass a }

(!?.) :: Attributable h => h -> (Bool, Text) -> h
h !?. (p, v) = if p then h !. v else h

key :: Int -> Attribute
key i = Attribute $ \a -> a { elementKey = Just i }

attribute :: Text -> Text -> Attribute
attribute k v = Attribute $ \a ->
    a { otherAttributes = HashMap.insert k v (otherAttributes a) }

boolean :: Text -> Attribute
boolean k = attribute k ""

-- onEvent' :: (Handler f, Event e) => EventType -> f e -> Attribute
-- onEvent' k handler = Attribute $ \a ->
--     a { handlers = HashMap.insert k h (handlers a) }
--   where
--     h v = do
--         e <- extractEvent v
--         fire handler e

onEvent :: (Handler f, Event e) => EventType -> f e -> Attribute
onEvent k handler = Attribute $ \a ->
    a { handlers = HashMap.insert k h (handlers a) }
  where
    h v = do
        e <- extractEvent v
        fire handler e