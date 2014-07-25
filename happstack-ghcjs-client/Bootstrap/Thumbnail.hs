{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Clckwrks.AVR3.Bootstrap.Thumbnail (thumbnail) where

import MyPrelude
import Clckwrks.AVR3.Bootstrap.Utils (classes)
import Network.URI (URI)
import Text.Blaze.Html5 as H ((!), a, Markup, ToValue(toValue))
import Text.Blaze.Html5.Attributes as H (href)

default (String)

thumbnail :: URI -> Markup -> Markup
thumbnail uri = H.a ! H.href (H.toValue $ show uri) ! classes ["thumbnail"]


