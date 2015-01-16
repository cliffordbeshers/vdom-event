{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Monoid
-- import Control.Monad.Trans
-- import Control.Monad.RWS.Lazy
import Control.Monad.State
-- import Control.Monad.Trans.State
import qualified GHCJSStub.JQuery as JQuery (on, Event(..), EventType(..), HandlerSettings, JQuery)
import Text.Blaze.Html5 (Markup, ToMarkup(..))
import Text.Blaze.Html5 as H (script, style)
import Data.Lens.Strict



data WM_Header = WMH_JavaScript String | WMH_CSS String deriving (Eq, Show)
data WM_Body = WMB_Initialization String deriving (Eq, Show)
data WebSite = WebSite { headers :: [WM_Header], bodies :: [WM_Body]} deriving Show
newtype WebSiteM m a = WebSiteM { unWebSiteM :: StateT WebSite m a }



wm_Header_toMarkup h =
  case h of
    WMH_JavaScript s -> H.script $ toMarkup s
    WMH_CSS s -> H.style $ toMarkup s

wm_Body_toMarkup b =
  case b of
    WMB_Initialization s -> H.script $ toMarkup s


instance ToMarkup WM_Header where
  toMarkup = wm_Header_toMarkup

splus :: WebSite-> WebSite-> WebSite
splus a b = WebSite { headers = headers a <> headers b, bodies = bodies a <> bodies b }

instance Monoid WebSite where
  mempty = WebSite [] []
  mappend = splus

instance Functor m => Functor (WebSiteM m) where
  fmap f = WebSiteM . fmap f . unWebSiteM

instance (Applicative m,  Monad m) => Applicative (WebSiteM m) where
  pure = WebSiteM . pure
  WebSiteM f <*> WebSiteM a = WebSiteM $ f <*> a

instance Monad m => Monad (WebSiteM m) where
  return = WebSiteM . return
  m >>= k = WebSiteM (unWebSiteM m  >>= unWebSiteM . k )

instance (Applicative m, MonadPlus m) => Alternative (WebSiteM m) where
  empty = WebSiteM mzero
  WebSiteM mzero <|> b = b
  a <|> _ = a

--instance MonadTrans WebSiteM where
--   lift m = WebSiteM m -- $ do
--     a <- m
--     return a
-- instance MonadTrans (StateT s) where
--     lift m = StateT $ \s -> do
--         a <- m
--         return (a, s)

tellHead :: Monad m => [WM_Header] -> WebSiteM m ()
tellHead hs = WebSiteM $ modify (f hs)
  where f xs (WebSite hs bs) = WebSite (hs ++ xs) bs

tellBody :: Monad m => [WM_Body] -> WebSiteM m ()
tellBody hs = WebSiteM $ modify (f hs)
  where f xs (WebSite hs bs) = WebSite hs (bs ++ xs)
  
wimport :: Monad m => WebSite -> a -> WebSiteM m a
wimport s bindings = do
  tellHead (headers s)
  tellBody (bodies s)
  return bindings

jquery :: Monad m => WebSiteM m JQueryBindings
jquery = wimport (WebSite [WMH_JavaScript "jquery import"] [WMB_Initialization "jquery initialization"]) jQueryBindings

data JQueryBindings = JQueryBindings { on :: (JQuery.Event -> IO ()) -> JQuery.EventType -> JQuery.HandlerSettings -> JQuery.JQuery -> IO (IO ()) }

jQueryBindings :: JQueryBindings
jQueryBindings = JQueryBindings { on = JQuery.on }

data BootStrap = BootStrap

bootstrap :: Monad m => WebSiteM m BootStrap
bootstrap = do
  WebSiteM $ modify $ splus (WebSite 
                          [WMH_JavaScript "bootstrap javascript import", WMH_CSS "bootstrap css import"] 
                          [WMB_Initialization "bootstrap initialization"])
  return BootStrap

-- modifyL :: Monad m => Lens s a -> (a -> a) -> WebSiteM m WebSite
modifyL lens f = WebSiteM $ modify (modL lens f)

website :: Monad m => WebSiteM m ()
website = do
  jq <- jquery
  bs <- bootstrap
  tellBody $ [WMB_Initialization "Hello, World"]
  return ()
  
website2 :: Monad m => WebSiteM m ()
website2 = do
  jq <- jquery
  bs <- bootstrap
  tellBody $ [WMB_Initialization "Goodbye, World"]
  return ()

x :: Monad m => WebSiteM m ()
x = website >> website2


runWebSiteM :: Monad m => WebSiteM m a -> (a,WebSite)
runWebSiteM m = runStateT (unWebSiteM m) mzeroWebSite
