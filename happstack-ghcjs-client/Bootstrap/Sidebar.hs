{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.AVR3.Bootstrap.Sidebar where

import MyPrelude
import Clckwrks.AVR3.Bootstrap.Blaze as H ((!), a, div, href, li, Markup, style, ul)
import Clckwrks.AVR3.Bootstrap.Utils (aref, classes, role)
import Clckwrks.AVR3.IdentSupply (Ident)

default (String)


sidebarNav :: [Markup] -> Markup
sidebarNav ms = 
  H.div ! classes ["bs-sidebar","hidden-desktop", "affix"] ! role "complementary" 
    ! style "overflow-y: auto; height: 100%;" $
  H.ul ! classes ["nav", "bs-sidenav"] $
  mapM_ (H.li ! classes []) ms

sidebarNav' :: [(Ident, Markup)] -> Markup
sidebarNav' pairs = 
  H.div ! classes ["bs-sidebar","hidden-desktop", "affix"] ! role "complementary"
    ! style "overflow-y: auto; height: 100%;" $
  H.ul ! classes ["nav", "bs-sidenav"] $
  mapM_ (H.li ! classes []) $ map (uncurry anchor) pairs
    where anchor ident lnk = H.a ! H.href (aref ident) $ lnk


