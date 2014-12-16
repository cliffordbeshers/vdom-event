{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fno-warn-orphans #-}
#if CLIENT
module WebModule.WebModuleM (WebSiteM) where
#else
module WebModule.WebModuleM 
       ( WebSiteM
       , wimport
       , tellServerPart
       , tellHead
       , tellBody
       , mzeroWebSite
       , mkWebSiteM
       , runWebSiteM
       , compileWebSiteM
       , mkTemplatePart
       , templateMarkup
       , defaultHandler
       , indexDotHtml
       , rootHandler
       , htmlHandler
       ) where
#endif


#if CLIENT
import Control.Monad.Trans.Identity
#endif    
#if SERVER
import Control.Monad as Monad (MonadPlus(mplus, mzero), msum)
import Control.Monad.Trans.Writer (tell, WriterT(runWriterT))
import Data.Monoid (Monoid(mappend, mempty))
import Data.Text as Text (pack)
import Happstack.Server as Happstack (dirs, nullDir, ok, Response, ServerPartT, ToMessage(..))
import Text.Blaze.Html5 (Markup, ToMarkup(toMarkup))
import WebModule.Markable (WM_Body, WM_Header)
import WebModule.Template (htmlTemplate)
import WebModule.WebModule (WebSite(..), wplus)
#endif

#if SERVER
type WebSiteM = WriterT WebSite
#else
type WebSiteM = IdentityT
#endif

#if SERVER

instance Monoid WebSite where
  mempty = mzeroWebSite
  mappend = wplus

tellServerPart :: Monad m => ServerPartT IO Response -> WebSiteM m ()
tellServerPart sp = tell $ mempty { serverpart = sp }

tellHead :: Monad m => [WM_Header] -> WebSiteM m ()
tellHead xs = tell $ mempty { headers = xs }

tellBody :: Monad m => [WM_Body] -> WebSiteM m ()
tellBody xs = tell $ mempty { bodies = xs }
  
wimport :: Monad m => WebSite -> a -> WebSiteM m a
wimport s bindings = do
  tell s
  return bindings


mkWebSiteM :: Monad m => WebSite -> WebSiteM m ()
mkWebSiteM ws = tell ws

mzeroWebSite :: WebSite
mzeroWebSite = WebSite { serverpart = mzero
                       , baseURL = []
                       , headers = []
                       , bodies = []
                       , manifest = []
                       }

runWebSiteM :: WriterT w m a -> m (a, w)
runWebSiteM  = runWriterT

-- This is probably mapWriterT composed with runWriterT or something.
compileWebSiteM :: Monad m => WebSiteM m a -> m (a, WebSite)
compileWebSiteM m = do
  (a,ws) <- runWebSiteM m
  return (a, f ws)
  where f :: WebSite -> WebSite
        f ws = ws { serverpart = mkTemplatePart ws `mplus` serverpart ws}

mkTemplatePart :: WebSite -> ServerPartT IO Response
mkTemplatePart ws = rootHandler (templateMarkup ws)

-- This is the sole html page pulled from the server, it includes all
-- the javascript, css, initialization code, etc. as well as a DOM
-- element which serves as the application root.

templateMarkup :: WebSite -> Markup
templateMarkup ws = htmlTemplate useManifest title (hs ws) (bs ws)
  where title = Text.pack "Need a title"
        hs = map toMarkup . headers
        bs = map toMarkup . bodies
        useManifest = False

defaultHandler :: ToMessage a => a -> ServerPartT IO Response
defaultHandler m = nullDir >> ok (toResponse m)

indexDotHtml :: ToMessage a => a -> ServerPartT IO Response
indexDotHtml = htmlHandler "index.html"

rootHandler :: ToMessage a => a -> ServerPartT IO Response
rootHandler m = msum [ defaultHandler m
                     , indexDotHtml m
                     ]
htmlHandler :: ToMessage a => FilePath -> a -> ServerPartT IO Response
htmlHandler fp m = dirs fp $ ok (toResponse m)
#endif
