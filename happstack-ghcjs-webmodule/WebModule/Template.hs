{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module WebModule.Template (htmlTemplate, htmlTemplateLucid) where

import Data.Text as Text (Text, pack)
import Text.Blaze.Html5 ((!), Markup, toMarkup, toValue)
import qualified Text.Blaze.Html5 as H (body, docTypeHtml, head, meta, title)
import qualified Text.Blaze.Html5.Attributes as HA (content, httpEquiv, manifest)
import WebModule.ManifestURL (manifestURL)
import Lucid

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
      H.body $ do sequence_ bodies

textshow :: Show a => a -> Text
textshow = Text.pack . show

htmlTemplateLucid :: Monad m =>
                 Bool ->          -- Include the manifest attribute
                 Text ->          -- ^ title , cannot contain markup.
                 [HtmlT m a] ->   -- ^ extra tags to include in \<head\>
                 [HtmlT m b] ->   -- ^ contents to put inside \<body\> 
                 HtmlT m ()
htmlTemplateLucid = htmlTemplate'

htmlTemplate' :: Monad m =>
                 Bool ->          -- Include the manifest attribute
                 Text ->          -- ^ title , cannot contain markup.
                 [HtmlT m a] ->   -- ^ extra tags to include in \<head\>
                 [HtmlT m b] ->   -- ^ contents to put inside \<body\> 
                 HtmlT m ()
htmlTemplate' useManifest title imports bodies =
  let manifest = if useManifest then [manifest_ (textshow manifestURL)] else [] in
  doctypehtml_ `with` manifest $ do
      head_ $ do
        meta_  [httpEquiv_ "Content-Type", content_ "text/html; charset=UTF-8" ]
        sequence_ imports
        title_ $ toHtml title
        body_ $ do sequence_ bodies
