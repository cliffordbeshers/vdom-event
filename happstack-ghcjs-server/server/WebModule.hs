{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module WebModule where

import Control.Monad as Monad (mplus)
import Control.Monad.Trans as Monad
import Data.Text as T (Text)
import "network-uri" Network.URI
import Data.ByteString as B (ByteString) -- Force Strict
import Text.Blaze.Html5 as H (Markup, ToValue(..), ToMarkup(..), link, (!), script, docTypeHtml, head, body)
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

data WebModuleM

data MimeType = MT_CSS | MT_Javascript| MT_HTML | MT_Favicon

data WebImport = WI MimeType URI B.ByteString

instance ToMarkup WebImport where
  toMarkup (WI MT_CSS uri content) =   
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href (toValue . show $ uri)
  toMarkup (WI MT_Javascript uri content) =   
    H.script ! HA.type_ "text/javascript" ! HA.src (toValue . show $ uri) $ return ()



data WebSite = WebSite { serverpart :: ServerPartT IO Response 
                       , baseURL :: [ModuleScopeURL]
                       , headMarkup :: Markup
                       , bodyMarkup :: Markup
                       , manifest :: [URI] -- ?
                       }

$(nameMakeLens ''WebSite (Just . (++ "Lens")))

runWebSite :: WebSite -> ServerPartT IO Response
runWebSite = serverpart

wsum :: WebSite -> WebSite -> WebSite
wsum a b = WebSite { serverpart = (serverpart a `mplus` serverpart b)
                   , baseURL = baseURL a ++ baseURL b
                   , headMarkup = headMarkup a >> headMarkup b
                   , bodyMarkup = bodyMarkup a >> bodyMarkup b
                   , manifest = []
                   }

homePage :: WebSite -> Markup
homePage ws = 
  H.docTypeHtml ! HA.manifest (toValue $ manifestURL) $ do
    H.head (headMarkup ws)
    H.body (bodyMarkup ws)
