{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-orphans #-}
module WebModule.WebModule ( WebSite(..)
                           , serverpartLens
                           , baseURLLens
                           , headersLens
                           , bodiesLens
                           , manifestLens
                           , wplus
                           , homePage
                           ) where

import Control.Monad as Monad (mplus)
import Data.Lens.Template (nameMakeLens)
import Data.List (nub)
import Data.Monoid ((<>))
import Data.Text as T (Text)
#if CLIENT
import ClientStub.Happstack.Server (Response, ServerPartT)
#else
import Happstack.Server (Response, ServerPartT)
#endif
import Network.URI (URI)
import Text.Blaze.Html5 as H ((!), body, docTypeHtml, head, link, Markup, script, ToMarkup(toMarkup), ToValue(toValue))
import qualified Text.Blaze.Html5.Attributes as HA (href, manifest, rel, src, type_)
import WebModule.Favicon (faviconMarkup)
import WebModule.ManifestURL (manifestURL)
import WebModule.Markable (WM_Body(..), WM_Header(..))
import WebModule.ModuleScopeURL (ModuleScopeURL, moduleScopeURLtoURI)

default (T.Text)

instance ToValue URI where
  toValue = toValue . show


--type WS = WebModule
--data WSE = WSE URI B.ByteString | WSN URI | WS_FALLBACK URI URI


--wse :: MimeType -> URI -> ByteString -> [WSE]
--wse mt u b = convert $ WebModule [WI mt u b]

-- convert :: WebModule -> [WSE]
-- convert (WebModule wis) = map cv wis
--   where cv (WI _ uri content) = WSE uri content

-- data WebModule = WebModule [WebImport]

-- newtype WebModuleM = WebModuleM { unWebModule :: WebModule }

-- data MimeType = MT_CSS | MT_Javascript | MT_Favicon

-- data WebImport = WI MimeType URI B.ByteString

-- instance ToMarkup WebImport where
--   toMarkup (WI MT_CSS uri _content) =
--     H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href (toValue . show $ uri)
--   toMarkup (WI MT_Javascript uri content) =
--     H.script ! HA.type_ "text/javascript" ! HA.src (toValue . show $ uri) $ return ()
--   toMarkup (WI MT_Favicon uri _content) = do
--     H.link 
--       ! HA.rel "shortcut icon" 
--       ! HA.href (toValue $ show uri)
--       ! HA.type_ "image/x-icon"
--     H.link 
--       ! HA.rel "icon" 
--       ! HA.href (toValue $ show uri)
--       ! HA.type_ "image/x-icon"

-- TODO "happstack-ghcjs-server" WebModule.WebModule.GuardServerPartsWithBaseURL
-- baseURL is explicitly encoded for several reasons, mostly web-routes related,
-- i.e., scoping etc., but it is also there for browsing purposes, so that pages
-- representing the data structures can be generated automatically.

-- TODO "happstack-ghcjs-server" WebModule.WebModule.RepresentPageStateMonad
-- as per discussion with Jeremy about ReactHaskell, applications of this sort
-- are really not URI based, they need an internal state from which the DOM tree
-- can be reproduced.  This will be useful for history, back button, sharing, etc.
-- 1) Test the basic mechanism
-- 2) make sure security is encoded somehow
-- 3) ideally it would support fragments, e.g., I could share part of a document.


data WebSite = WebSite { serverpart :: ServerPartT IO Response 
                       , baseURL :: [ModuleScopeURL]
                       , headers :: [WM_Header]
                       , bodies :: [WM_Body]
                       , manifest :: [URI] -- ?
                       }

instance Show WebSite where
  show = const "WebSite { } show unimplemented"

wm_Header_toMarkup :: WM_Header -> Markup
wm_Header_toMarkup h =
  case h of
    WMH_JavaScript u -> H.script ! HA.type_ "application/javascript" ! HA.src (toValue u) $ return ()
    WMH_CSS u -> H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href (toValue u)
    WMH_Favicon uri -> faviconMarkup (moduleScopeURLtoURI uri)

wm_Body_toMarkup :: WM_Body -> Markup
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
