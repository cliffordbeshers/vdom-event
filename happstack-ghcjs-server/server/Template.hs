{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Template (WebImport(..), htmlTemplate) where

import Prelude as P
import Data.Text (Text)
import Text.Blaze.Html5 ((!), Markup, toMarkup)
import qualified Text.Blaze.Html5 as H (body, docTypeHtml, head, link, meta, title)
import qualified Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, rel, type_)

default (Text)

-- TODO: Instead, adopt a monoidal ([header],[body]) model.
-- See Gershom Bazerman's code.

data WebImport = Header Markup | JavaScript Markup

headers imps = [ m | (Header m) <- imps]
scripts imps = [ m | (JavaScript m) <- imps ]

htmlTemplate :: Text       -- ^ title , cannot contain markup.
                -> [WebImport]  -- ^ extra tags to include in \<head\>
                -> [Markup]  -- ^ contents to put inside \<body\> 
                -> Markup
htmlTemplate title imports bodies =  do 
  H.docTypeHtml $ do
      H.head $ do
        H.meta ! HA.httpEquiv "Content-Type" ! HA.content "text/html; charset=UTF-8"
        H.link 
          ! HA.rel "shortcut icon" 
          ! HA.href "/favicon.ico" 
          ! HA.type_ "image/x-icon"
        H.link 
          ! HA.rel "icon" 
          ! HA.href "/favicon.ico" 
          ! HA.type_ "image/x-icon"
        sequence_ $ headers imports
        H.title (toMarkup title)
      H.body $ do sequence_ bodies
                  sequence_ $ scripts imports
    
