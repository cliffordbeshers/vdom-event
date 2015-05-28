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

hello :: MVar (Bool -> Bool) -> Bool -> Html
hello  redrawChannel b = let inputText = "Hello this is in put" in
  div_ !# (if b then "Hello Header" else "Goodbye Header") $ do
      h1_ ! clicker $ (text_ "Click to reverse")
      mkButton (text_ "Click to reverse") ! clicker >> br_
      mkCheckbox "Check1" "Check2" >> (text_ "Check") >> br_
      textarea b bigText ! onInput'
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

textarea :: Bool -> Text -> Html
textarea b t = form' (xform b t)
  where xform b = if b then id else Text.reverse
        form' :: Text -> Html
        form' t = 
          textarea_ !# "new-hello"
            ! autofocus_
            ! cols_ (tshow (80 :: Int))
            ! rows_ (tshow (10 :: Int))
            ! focus'
            ! blur'
            $ text_ t
        focus' = onFocus (Inputt (\e -> print ("hello focus", e)))
        blur' = onBlur (Inputt (\e -> print ("hello blur", e)))

mkButton :: Html -> Html
mkButton = button_ ! type_ "button"

mkKeygen :: Text -> Html
mkKeygen nm = keygen_ ! name_ nm

mkCheckbox :: Text -> Text -> Html
mkCheckbox nm v = input_
  ! type_ "checkbox"
  ! name_ nm
  ! value_ v


instance Handler Inputt where

    fire (Inputt m) = m

newtype Inputt e = Inputt (e -> IO ())

onClick :: Handler f => f E.MouseEvent -> Attribute
onClick = onEvent Click

onDoubleClick :: Handler f => f E.MouseEvent -> Attribute
onDoubleClick = onEvent DoubleClick

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
