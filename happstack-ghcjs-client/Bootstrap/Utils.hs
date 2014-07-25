{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.AVR3.Bootstrap.Utils 
       ( aref
       , classes
       , classesT
       , role
       , toggle
       , target
       ) where

import MyPrelude
import Clckwrks.AVR3.Bootstrap.Blaze as H (Attribute, AttributeValue, class_, customAttribute, dataAttribute, div, ToValue(toValue))
import Clckwrks.AVR3.IdentSupply (Ident)
import Data.List (intercalate)
import qualified Data.Text as T (Text, intercalate, pack)

default (String)

aref :: Ident -> H.AttributeValue
aref = H.toValue . ("#" ++)

zbox b xs = H.div $ "zbox"

role = H.customAttribute "role"
toggle = H.dataAttribute "toggle"
target = H.dataAttribute "target"

classes :: [String] -> H.Attribute
classes = H.class_ . H.toValue . intercalate " "

classesT :: [T.Text] -> H.Attribute
classesT = H.class_ . H.toValue . T.intercalate (T.pack " ")


