{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.AVR3.Bootstrap.Well (well) where

import MyPrelude
import Clckwrks.AVR3.Bootstrap.Blaze as H ((!), a, div, href, li, Markup, ul, Attributable)
import Clckwrks.AVR3.Bootstrap.Utils (aref, classesT, role)
import Clckwrks.AVR3.IdentSupply (Ident)
import Data.Text (Text)
import Data.Tree
import Clckwrks.AVR3.ZipTree
import Clckwrks.AVR3.Bootstrap.Blaze 

default (Text)

well :: Markup -> Markup
well body =
  H.div ! classesT ["well", "well-default"] $ body
