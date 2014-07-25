{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AdminConsole where -- (AdminConsoleURL(..), adminConsole) where

-- import Appraisal.Report
-- import Appraisal.ReportImage (ReportImage)
-- import Appraisal.ReportMap
-- import Appraisal.Utils.UUID as UUID (UUID)
-- import Appraisal.Permissions (Permissions(Permissions, owner, writers, readers))
-- import AppraisalDef
-- import AppraisalData
-- import Clckwrks.AVR3.Monad
-- import Clckwrks
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html (Html)

import Control.Monad.Reader
--import Data.Acid as A
import Data.Data (Data)
import Data.List (intercalate)
--import qualified Data.Text as T (Text)
import Data.Typeable (Typeable)
--import qualified Text.Blaze.Html5 as H
--import qualified Text.Blaze.Html5.Attributes as HA (class_, type_, placeholder, id, tabindex, href)
--import Text.Blaze.Html5 ((!))
--import Text.Blaze.Html.Renderer.Pretty (renderHtml)
--import Acid
--import Config
import Data.Tree
--import ZipTree
--import Bootstrap
--import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Prelude as P

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 as H ((!))
import qualified Text.Blaze.Html5.Attributes as HA


default (String)


content = pdfEmbed "http://cs.au.dk/~hosc/local/LaSC-7-1-pp39-56.pdf"
pdfEmbed :: String -> H.Markup
pdfEmbed u = H.embed ! HA.width "100%" ! HA.height "600px" 
                ! HA.name "plugin" ! HA.src (H.toValue u) 
                ! HA.type_ "application/pdf"

pdfEmbedIframe :: String -> H.Markup
pdfEmbedIframe u = H.iframe ! HA.id "iframepdf" ! HA.style "width: 100%" ! HA.src (H.toValue u) $ ""

pdfCanvasUnfinished = H.div $ H.canvas ! HA.id "the-canvas" ! HA.style "border:1px solid black;" $ ""

-- adminConsole :: MonadIO m => AdminConsoleURL -> AVR3T AdminConsoleURL m H.Markup
-- adminConsole u = do
--   AVR3Config c a <- lift $ ask
--   ReportMap rmap <- liftIO $ A.query (appraisalData a) QueryReportMap
--   m <- return $ "adminConsole should be here" -- liftIO $ aslayout
--   return $ m
  -- return $ container $ [ H.toMarkup $ markupConf (httpConf c)
  --                      , xxx
  --                      ]

