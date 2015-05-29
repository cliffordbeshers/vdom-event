{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}


import Control.Concurrent
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Data.Default
import Data.Monoid
import Data.Text as Text (Text, pack, reverse, concat)
import Data.Time
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

default (Text.Text)


data R = R
data W = W
data S = S

instance Monoid W where
  mempty = W
  W `mappend` W = W

type Z a = RWST R W S a


type URL = Text

imageURLs :: [URL]
imageURLs =
  [ "https://fbcdn-sphotos-e-a.akamaihd.net/hphotos-ak-xaf1/t31.0-8/11082361_10204865703177218_7142048938861798417_o.jpg"
  , "https://scontent-ord.xx.fbcdn.net/hphotos-xpa1/v/t1.0-9/s720x720/10676351_10204875971793927_291876182821710665_n.jpg?oh=77cf176c31c46ac933272de0480dcfe1&oe=55818649"
  , "https://fbcdn-sphotos-c-a.akamaihd.net/hphotos-ak-xpf1/t31.0-8/10914721_10204856412024945_2607390891971097151_o.jpg"
  , "https://scontent-ord.xx.fbcdn.net/hphotos-xpf1/v/t1.0-9/10988282_10204849112762468_7979569982779097370_n.jpg?oh=712dc71ac4a1a5c7a6d452ec57d6799f&oe=5583185E"
  , "https://scontent-ord.xx.fbcdn.net/hphotos-xfp1/t31.0-8/11073500_10204842190469415_875697331045021877_o.jpg"
  , "https://fbcdn-sphotos-g-a.akamaihd.net/hphotos-ak-xpa1/v/t1.0-9/10710956_10204829354548525_3840378263239480270_n.jpg?oh=a6f915cf68b916c6c84c6e8baeecb028&oe=55B52B34&__gda__=1434306594_f075e1695ab5a3a56ff0a6d7921b6254"
  , "https://scontent-ord.xx.fbcdn.net/hphotos-xpf1/v/t1.0-9/10983182_10204802735643069_2375287011727111812_n.jpg?oh=c57d524c6be33146eb175548a7f46200&oe=5573FFE1"
  , "https://fbcdn-sphotos-d-a.akamaihd.net/hphotos-ak-prn2/v/t1.0-9/11056894_10204795437740626_1302633900976191151_n.jpg?oh=da776a52e13eba9c1426b03708f1fd15&oe=557A2D8C&__gda__=1437981455_ed50f8cf7a166212a9e087b81fb3cd5c"
  ]


hello :: MVar (Bool -> Bool) -> Bool -> Html
hello  redrawChannel b = let inputText = "Hello this is in put" in
  div_ !# (if b then "Hello Header" else "Goodbye Header") $ do
      h1_ ! clicker $ (text_ "Click to reverse")
      mkButton (text_ "Click to reverse") ! clicker >> br_
      textarea b bigText ! onInput' >> br_
      mkCheckbox "Check1" "Check2" >> (text_ "Check") >> br_
      mkSelect "menu1" [("1","One"),("2","Two"),("3","Three")] "3" >>  br_
      mapM_ (\u -> mkImg u 50 50 >> br_) imageURLs
      
  where clicker = onClick (Inputt (\e -> print "clicker" >> putMVar redrawChannel not))
        onInput' = onInput (Inputt (\e -> print "input" >> putMVar redrawChannel id))
        bigText = Text.concat $ replicate 10 "This is the input Text. "

textform :: Bool -> Text -> Html
textform b t = form' (xform b t)
  where xform b = if b then id else Text.reverse
        form' :: Text -> Html
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

textarea :: Bool -> Text -> Html
textarea b t = form' (xform b t)
  where xform b = if b then id else Text.reverse
        form' :: Text -> Html
        form' t = 
          textarea_ !# "new-hello"
            ! autofocus_
--            ! cols_ (tshow (80 :: Int))
--            ! rows_ (tshow (10 :: Int))
            ! A.style_ "min-width:30%;width:50%;max-width:80%;height:10em;"
            ! focus'
            ! blur'
            ! dragEnd'
            ! mouseUp'
            $ text_ t
        focus' = onFocus (Inputt (\e -> print ("hello focus", e)))
        blur' = onBlur (Inputt (\e -> print ("hello blur", e)))
        dragEnd' = onDragEnd (Inputt (\e -> print ("received DragEnd", e)))
        mouseUp' = onMouseUp (Inputt (\e -> print ("received mouseUp", e)))

mkButton :: Html -> Html
mkButton = button_ ! type_ "button"

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

alderon :: (MVar (Bool -> Bool) -> Bool -> Html) -> IO ()
alderon html = do
  loadBootstrap
  root <- documentBody
  redrawChannel <- newMVar id
  eventLoop root redrawChannel html True


eventLoop root redrawChannel render state = do
  update <- takeMVar redrawChannel
  let state' = update state
  hs <- buildDOM (render redrawChannel state')
  old <- detachChildren root
  appendChildren root hs
  eventLoop root redrawChannel render state'



main = do
  alderon hello
