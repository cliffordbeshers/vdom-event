{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Template (WebImport(..), htmlTemplate, htmlTemplate') where

import Prelude as P
import Data.Text (Text)
import Text.Blaze.Html5 ((!), Markup, toMarkup, toValue)
import qualified Text.Blaze.Html5 as H (body, docTypeHtml, head, link, meta, title)
import qualified Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, manifest, rel, type_)
import Favicon
import ManifestURL (manifestURL)

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
  H.docTypeHtml ! HA.manifest (toValue $ show manifestURL) $ do
      H.head $ do
        H.meta ! HA.httpEquiv "Content-Type" ! HA.content "text/html; charset=UTF-8"
        sequence_ $ headers imports
        H.title (toMarkup title)
      H.body $ do sequence_ bodies
                  sequence_ $ scripts imports
    
htmlTemplate' :: Text       -- ^ title , cannot contain markup.
                -> [Markup]  -- ^ extra tags to include in \<head\>
                -> [Markup]  -- ^ contents to put inside \<body\> 
                -> Markup
htmlTemplate' title imports bodies =  do 
  H.docTypeHtml {- ! HA.manifest (toValue $ show manifestURL) -} $ do
      H.head $ do
        H.meta ! HA.httpEquiv "Content-Type" ! HA.content "text/html; charset=UTF-8"
        sequence_ imports
        H.title (toMarkup title)
      H.body $ sequence_ bodies
    
