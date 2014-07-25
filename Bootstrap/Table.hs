{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Clckwrks.AVR3.Bootstrap.Table (table
                                     , TableOption(..)
                                     ) where

import MyPrelude
-- Import this directly since we do not want to export it from Blaze
import Clckwrks.AVR3.Bootstrap.Blaze as H ((!), Markup)
import Clckwrks.AVR3.Bootstrap.Utils (classes)
import qualified Text.Blaze.Html5 as H (table)
import Data.Char (toLower)

default (String)

data TableOption = Striped | Bordered | Hover | Condensed 
                 deriving (Eq, Ord, Read, Show)

optionToClass :: TableOption -> [Char]
optionToClass = ("table-" ++) . map toLower . show


table :: [TableOption] -> Markup -> Markup
table opts = H.table ! classes (["table"] ++ map optionToClass opts)


