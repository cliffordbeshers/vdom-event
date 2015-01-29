{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.AVR3.Bootstrap.BreadCrumbs (breadcrumbs) where

import MyPrelude
import Clckwrks.AVR3.Bootstrap.Blaze as H ((!), ol, li)
import Clckwrks.AVR3.Bootstrap.Utils (aref, classesT, role)
import Clckwrks.AVR3.IdentSupply (Ident)
import Data.Text (Text)
import Data.Tree
import Clckwrks.AVR3.ZipTree
import Clckwrks.AVR3.Bootstrap.Blaze 
import Clckwrks.AVR3.Bootstrap.Keyword

default (Text)

-- This keyword type may seem like overkill, but getting "breadcrumbs"
-- instead of "breadcrumb" ruins your whole website.

data BreadCrumbsClass = BreadCrumbsClass
instance Keyword BreadCrumbsClass where
  keyword BreadCrumbsClass = "breadcrumb"

breadcrumbs :: [Markup] -> Markup
breadcrumbs bs =
  H.ol ! classesT [keyword BreadCrumbsClass] $ mapM_ H.li bs


