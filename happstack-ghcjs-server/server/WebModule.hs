{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module WebModule where

import Control.Monad as Monad (mplus)
import Control.Monad.Trans as Monad
import Data.List (nub)
import Data.Monoid ((<>))
import Data.Text as T (Text)
import "network-uri" Network.URI
import Data.ByteString as B (ByteString) -- Force Strict
import Text.Blaze.Html5 as H (Markup, ToValue(..), ToMarkup(..), link, (!), script, docTypeHtml, head, body, style)
import qualified Text.Blaze.Html5.Attributes as HA (href, rel, src, type_, manifest)
import Text.Blaze.Html.Renderer.Utf8  (renderHtml)
import Happstack.Server (ServerPartT, Response)
import ModuleScopeURL
import ManifestURL (manifestURL)
import Data.Lens.Template (nameMakeLens)

default (T.Text)

instance ToValue URI where
  toValue = toValue . show

type WS = WebModule
data WSE = WSE URI B.ByteString | WSN URI | WS_FALLBACK URI URI


wse mt u b = convert $ WebModule [WI mt u b]

convert :: WebModule -> [WSE]
convert (WebModule wis) = map cv wis
  where cv (WI _ uri content) = WSE uri content

data WebModule = WebModule [WebImport]

newtype WebModuleM = WebModuleM { unWebModule :: WebModule }

data MimeType = MT_CSS | MT_Javascript| MT_HTML | MT_Favicon

data WebImport = WI MimeType URI B.ByteString

instance ToMarkup WebImport where
  toMarkup (WI MT_CSS uri content) =   
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href (toValue . show $ uri)
  toMarkup (WI MT_Javascript uri content) =   
    H.script ! HA.type_ "text/javascript" ! HA.src (toValue . show $ uri) $ return ()

data WM_Header = WMH_JavaScript String | WMH_CSS String deriving (Eq, Show)
data WM_Body = WMB_Initialization String deriving (Eq, Show)

data WebSite = WebSite { serverpart :: ServerPartT IO Response 
                       , baseURL :: [ModuleScopeURL]
                       , headers :: [WM_Header]
                       , bodies :: [WM_Body]
                       , manifest :: [URI] -- ?
                       }

instance Show WebSite where
  show = const "WebSite { } show unimplemented"

wm_Header_toMarkup h =
  case h of
    WMH_JavaScript s -> H.script $ toMarkup s
    WMH_CSS s -> H.style $ toMarkup s

wm_Body_toMarkup b =
  case b of
    WMB_Initialization s -> H.script $ toMarkup s

instance ToMarkup WM_Header where
  toMarkup = wm_Header_toMarkup

instance ToMarkup WM_Body where
  toMarkup = wm_Body_toMarkup


$(nameMakeLens ''WebSite (Just . (++ "Lens")))


wplus :: WebSite -> WebSite -> WebSite
wplus a b = WebSite { serverpart = (serverpart a `mplus` serverpart b)
                    , baseURL = baseURL a `mplus` baseURL b
                    , headers = headers a <> headers b
                    , bodies = bodies a <> bodies b
                    , manifest = manifest a `mplus` manifest b
                    }

homePage :: WebSite -> Markup
homePage ws = 
  H.docTypeHtml ! HA.manifest (toValue $ manifestURL) $ do
    H.head $ mapM_ toMarkup (nub $ headers ws)
    H.body $ mapM_ toMarkup (nub $ bodies ws)
