{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}


import Control.Concurrent
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Data.Default
import Data.Monoid
import Data.Text as Text (Text, pack, reverse)
import Data.Time
import GHCJS.Foreign
import GHCJS.Foreign.QQ
import GHCJS.Types

import GHCJS.VDOM
import GHCJS.VDOM.QQ
import ControlMonadSupplyExcept
import Bootstrap

import MicroDOM
import BuildDOM

import Alderon.Html
import Alderon.Html.Internal
import Alderon.Html.Events as E (MouseEvent)

default (Text.Text)


hello :: MVar (Bool -> Bool) -> Bool -> Html
hello  redrawChannel b = let inputText = "Hello this is in put" in
  div_ !# (if b then "Hello Header" else "Goodbye Header") $ do
      h1_ ! clicker $ (text_ "Click to reverse")
      textform b "This is the input Text." ! onInput'
  where clicker = onClick (Inputt (\e -> print "clicker" >> putMVar redrawChannel not))
        onInput' = onInput (Inputt (\e -> print "input" >> putMVar redrawChannel id))

textform :: Bool -> Text -> Html
textform b t = form' (xform b t)
  where xform b = if b then id else Text.reverse
        form' t = 
          input_ !# "new-hello"
            ! placeholder_ "Hi, how are you?"
            ! autofocus_
            ! value_ t
            ! focus'
            ! blur'
        focus' = onFocus (Inputt (\e -> print ("hello focus", e)))
        blur' = onBlur (Inputt (\e -> print ("hello blur", e)))

instance Handler Inputt where
    fire (Inputt m) = m

newtype Inputt e = Inputt (e -> IO ())

onClick :: Handler f => f E.MouseEvent -> Attribute
onClick = onEvent Click

onDoubleClick :: Handler f => f E.MouseEvent -> Attribute
onDoubleClick = onEvent DoubleClick

alderon :: (MVar (Bool -> Bool) -> Bool -> Html) -> IO ()
alderon html = do
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
