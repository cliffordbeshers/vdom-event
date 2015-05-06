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

default (Text.Text)

hello :: Html
hello  = let inputText = "Hello" in
  div_ !# "Hello Header" $ do
    div_ !# "Goodbye Header" $ do
      h1_ $ text_ "Hellos"
      input_ !# "new-hello"
        ! placeholder_ "Hi, how are you?"
        ! autofocus_
        ! value_ inputText

alderon :: Html -> IO ()
alderon html = do
  root <- documentBody
  h' <- buildDOM html
  appendChildren root h'
  

  
main = alderon hello
