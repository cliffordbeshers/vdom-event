{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}


import Control.Concurrent
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Data.Default
import Data.Monoid
import Data.Text as Text (Text, pack, reverse, concat)
import Data.Time
import Documentation
import GHCJS.Foreign
import GHCJS.Types

import ControlMonadSupplyExcept
import Bootstrap

import MicroDOM
import BuildDOM

import Alderon.Html
import Alderon.Html.Internal
import qualified Alderon.Html.Attributes as A (style_)
import Alderon.Html.Events as E (MouseEvent)
import History
import Utils
import SampleImages as Samples (imageURLs)

default (Text.Text)


data R = R
data W = W
data S = S

instance Monoid W where
  mempty = W
  W `mappend` W = W

type Z a = RWST R W S a

data ModelTab = This | That deriving (Show, Bounded, Enum, Eq)

data Model = Model ModelTab Text

data WH = WH Int Int
data View = View { wh :: WH }
type AppState = (View, Model)

hello :: MVar ( AppState -> AppState) -> AppState -> Html
hello  redrawChannel (view, Model tab inputText) = let b = if tab == This then True else False in
  div_ !# "top" $ do
      h1_ ! clicker $ (text_ (if tab == This then "This Header" else "That Header"))
      mkButton (text_ "Click to reverse") ! clicker >> br_
      textarea (if b then inputText else Text.reverse inputText) ! onInput' ! mouseUp' >> br_
      mkCheckbox "Check1" "Check2" >> (text_ "Check") >> br_
      mkSelect "menu1" [("1","One"),("2","Two"),("3","Three")] "3" >>  br_
      mapM_ (\u -> mkImg u 50 50 >> br_) Samples.imageURLs
  where clicker = onClick (Inputt (\e -> print "clicker" >> putMVar redrawChannel clickerF))
        onInput' = onInput (Inputt (\e -> print "input" >> putMVar redrawChannel id))
        mouseUp' = onMouseUp (Inputt (\e -> print ("received mouseUp", e)))
        clickerF (v, Model tab text) = (v, Model (cycleEB tab) text)

cycleEB :: (Eq a, Bounded a, Enum a) => a -> a
cycleEB a = if a == maxBound then minBound else succ a


textform :: Text -> Html
textform = form'
  where form' :: Text -> Html
        form' t = 
          input_ !# "new-hello"
            ! placeholder_ "Hi, how are you?"
            ! autofocus_
            ! value_ t
            ! focus'
            ! blur'
        focus' = onFocus (Inputt (\e -> print ("hello focus", e)))
        blur' = onBlur (Inputt (\e -> print ("hello blur", e)))

-- Notes on textarea you do not have to use rows,cols, you can use css
-- width/height.  The specifies both the default and the minimum.
-- Playing with min-width and max-width as well doesn't seem to fix
-- that.

-- mouseUp event notifies of the resize, at least in chrome.
-- AS should trap and remember for all text widths.  At least until we
-- get full wysiwyg going.

textarea :: Text -> Html
textarea = form'
  where form' :: Text -> Html
        form' t = 
          textarea_ !# "new-hello"
            ! autofocus_
--            ! cols_ (tshow (80 :: Int))
--            ! rows_ (tshow (10 :: Int))
            ! A.style_ "min-width:fill-available;max-width:80%;min-height:fill-available;"
            ! focus'
            ! blur'
            $ text_ t
        focus' = onFocus (Inputt (\e -> print ("hello focus", e)))
        blur' = onBlur (Inputt (\e -> print ("hello blur", e)))

mkButton :: Html -> Html
mkButton = button_ ! type_ "button"

type URL = Text

mkImg :: URL -> Int -> Int -> Html
mkImg url w h =
  img_ ! src_ url ! width_ (tshow w) ! height_ (tshow h)

mkKeygen :: Text -> Html
mkKeygen nm = keygen_ ! name_ nm

mkCheckbox :: Text -> Text -> Html
mkCheckbox nm v = input_
  ! type_ "checkbox"
  ! name_ nm
  ! value_ v

mkSelect :: Text -> [(Text,Text)] -> Text -> Html
mkSelect nm vs v =
  select_ ! name_ nm $ mapM_ opt' vs
    where opt' (val,lbl) = if val == v
                           then option_ ! value_ val ! selected_ "" $ text_ lbl
                           else option_ ! value_ val $ text_ lbl


instance Handler Inputt where

    fire (Inputt m) = m

newtype Inputt e = Inputt (e -> IO ())

onClick :: Handler f => f E.MouseEvent -> Attribute
onClick = onEvent Click

onDoubleClick :: Handler f => f E.MouseEvent -> Attribute
onDoubleClick = onEvent DoubleClick

onDragEnd :: Handler f => f E.MouseEvent -> Attribute
onDragEnd = onEvent DragEnd

alderon :: (MVar (a -> a) -> a -> Html) -> a -> IO ()
alderon html s0 = do
  loadBootstrap
  root <- documentBody
  redrawChannel <- newMVar id
  eventLoop root redrawChannel html s0


eventLoop root redrawChannel render state = do
  update <- takeMVar redrawChannel
  let state' = update state
  hs <- buildDOM (render redrawChannel state')
  old <- detachChildren root
  appendChildren root hs
  eventLoop root redrawChannel render state'



main = do
  alderon hello (View (WH 60 60), Model This "Model text.")
