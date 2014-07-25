module Clckwrks.AVR3.Bootstrap.Blaze 
       ( H.a
       , HA.tabindex
       , H.div
       , H.p
       , HA.id
       , HA.placeholder
       , H.nav
       , H.li
       , H.ul
       , (H.!)
       , HA.href
       , H.label
       , H.form
       , H.input
       , H.span
       , H.table
       , H.textarea
       , H.tr
       , H.td
       , HA.type_
       , HA.style
       , H.button
       , H.Markup
       , ol
       , linkify
       , renderHtml
       , H.AttributeValue
       , H.Attribute
       , H.Attributable
       , H.ToValue(..)
       , H.ToMarkup(..)
       , HA.class_
       , H.customAttribute
       , H.dataAttribute
       ) where 
-- This module serves solely as a convenience for importing Text.Blaze.Html5 functions
-- in a standard way.

import MyPrelude (($))
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Internal as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Network.URI (URI)

ol :: H.Markup -> H.Markup
ol = H.customParent (H.stringTag "ol")

linkify :: H.ToValue a => a -> H.Markup -> H.Markup
linkify uri = H.a ! HA.href (H.toValue $ uri) 