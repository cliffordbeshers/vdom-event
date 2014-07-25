{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
module Application (avr3AllPages, avr3Application, avr3Harness, htmlTemplate) where

import Clckwrks (FilterMonad, Response, RouteT, showURL)
import qualified Clckwrks.AVR3.BootstrapModule as Bootstrap (imports)
import qualified Clckwrks.AVR3.CustomCSS as CustomCSS (imports)
import qualified Clckwrks.AVR3.JQuery as JQuery (imports)
import Clckwrks.AVR3.Template (htmlTemplate)
import Clckwrks.AVR3.URL (allPages, AVR3URL(Bootstrap, Bootstrap3Wysihtml5))
import Clckwrks.AVR3.Bootstrap3Wysihtml5 as B3W5 (imports)
import Clckwrks.AVR3.LiveDevel as LiveDevel (imports)
import Data.Text (Text)
import MyPrelude (($), (++), (.), map, mapM, mapM_, Monad(return), Show(show), zip)
import "mtl" Control.Monad.Trans (MonadIO)
import Text.Blaze.Html5 ((!), Markup)
import qualified Text.Blaze.Html5 as H (a, li, ToMarkup(toMarkup), ToValue(toValue), ul)
import qualified Text.Blaze.Html5.Attributes as HA (href)

default (Text)

avr3Application :: (MonadIO m, FilterMonad Response m) => RouteT AVR3URL m Markup
avr3Application = do
  let jis = []
  bis <- Bootstrap.imports Bootstrap
  let wis = []
  customCSS <- CustomCSS.imports
  wysihtml5is <- B3W5.imports Bootstrap3Wysihtml5
  livedevels <- LiveDevel.imports
  let imps = jis ++ bis ++ wis ++ customCSS ++ wysihtml5is ++ livedevels
  -- wishList' <- wishList
  return $ htmlTemplate "AppraisalScribe v3" imps []

avr3Harness :: MonadIO m => [Markup] -> RouteT AVR3URL m Markup
avr3Harness bodyparts = do
  jis <- JQuery.imports
  bis <- Bootstrap.imports Bootstrap
  let wis = []
  customCSS <- CustomCSS.imports
  wysihtml5is <- B3W5.imports Bootstrap3Wysihtml5
  livedevels <- LiveDevel.imports
  let imps = jis ++ bis ++ wis ++ customCSS  ++ wysihtml5is ++ livedevels
  -- wishList' <- wishList
  return $ htmlTemplate "AppraisalScribe v3" imps bodyparts

avr3AllPages :: MonadIO m => RouteT AVR3URL m Markup
avr3AllPages = do
  urls <- mapM showURL allPages
  let names = map show allPages
  avr3Harness $ ((:[]) . H.ul . mapM_ fmt) (zip urls names)
  where fmt (u, n) = H.li $ (H.a ! HA.href (H.toValue u)) $ H.toMarkup n

