{-# LANGUAGE OverloadedStrings #-}
module WebModule.Template (htmlTemplate) where

import Data.Text (Text)
import Text.Blaze.Html5 ((!), Markup, toMarkup, toValue)
import qualified Text.Blaze.Html5 as H (body, docTypeHtml, head, meta, title)
import qualified Text.Blaze.Html5.Attributes as HA (content, httpEquiv, manifest)
import WebModule.ManifestURL (manifestURL)

default (Text)

-- TODO: Instead, adopt a monoidal ([header],[body]) model.
-- See Gershom Bazerman's code.

-- data WebImport = Header Markup | JavaScript Markup

htmlTemplate :: Bool -> -- Use the manifest
                Text       -- ^ title , cannot contain markup.
                -> [Markup]  -- ^ extra tags to include in \<head\>
                -> [Markup]  -- ^ contents to put inside \<body\> 
                -> Markup
htmlTemplate mfest title imports bodies =
  (if mfest then H.docTypeHtml ! HA.manifest (toValue $ show manifestURL) else H.docTypeHtml) $
    do
      H.head $ do
        H.meta ! HA.httpEquiv "Content-Type" ! HA.content "text/html; charset=UTF-8"
        sequence_ imports
        H.title (toMarkup title)
      H.body $ sequence_ bodies
    
