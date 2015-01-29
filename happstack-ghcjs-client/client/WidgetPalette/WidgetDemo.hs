module WidgetPalette.WidgetDemo where

import Data.Text
import WebModule.WebModuleM
import Lucid
import Bootstrap.Tabs

data WidgetDemo m = WidgetDemo { description :: Text
                               , label :: Text
                               , body :: HtmlT m () -- Wrong, but twill serve
                               }
