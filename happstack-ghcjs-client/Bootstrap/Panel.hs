{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.AVR3.Bootstrap.Panel (panel) where

import MyPrelude
import Clckwrks.AVR3.Bootstrap.Blaze as H ((!), a, div, href, li, Markup, ul, Attributable)
import Clckwrks.AVR3.Bootstrap.Utils (aref, classesT, role)
import Clckwrks.AVR3.IdentSupply (Ident)
import Data.Text (Text)
import Data.Tree
import Clckwrks.AVR3.ZipTree
import Clckwrks.AVR3.Bootstrap.Blaze 

default (Text)

panel :: Markup -> Markup -> Markup
panel title body =
  H.div ! classesT ["panel", "panel-default"] $ do
    H.div ! classesT ["panel-heading"] $ title
    H.div ! classesT ["panel-body"] $ body

-- Hey, this type checks!  This could be nice.
panelZT :: Tree (Markup -> Markup)
panelZT = structure <!> (infiniteTree classesT <*> cs)
          -- <*> (infiniteTree af <*> cs)
  where structure :: Tree (Markup -> Markup)
        structure = Node H.div [Node H.div [], Node H.div []]
        cs :: Tree [Text]
        cs = Node ["panel", "panel-default"] 
             [ Node ["panel-heading"] []
             , Node ["panel-body"] []]
        (<*>) = applyT
        mt <!> at = infiniteTree (!) `applyT`  mt `applyT` at