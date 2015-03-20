{-# LANGUAGE OverloadedStrings #-}
module Bootstrap.Utils 
       ( aref
       , aria_controls_
       , classes_
       , role_
       , textshow
       , toggle_
       ) where

import Lucid
import Data.Monoid ((<>))
import Data.Text as Text (Text, intercalate, pack)
import Data.Set as Set

default (Text)

aref :: Text -> Text
aref = ("#" <>)

-- zbox b xs = div_ $ "zbox"

aria_controls_ :: Text -> Attribute
aria_controls_ = term "aria-controls"

role_ :: Text -> Attribute
role_ = term "role"
toggle_ :: Text -> Attribute
toggle_ = data_ "toggle"

classes_ :: [Text] -> Attribute
classes_ = class_ . Text.intercalate " " . Set.toList . Set.fromList

textshow :: Show a => a -> Text
textshow = Text.pack . show

