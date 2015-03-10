{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module WebModule.AJAXModule (ajaxModuleGen, AJAXBindings(..), AJAXType(..)) where

-- TODO WebModule.AJAXModule.GetAJAXConnectedToSortable
-- 1) I have never connected this code to actual UI
-- There are namespace issues that fall into the web routes problem
-- ignore those
-- get this connected and writing files
-- 2) create the file handle in main, as we would an acid-state
--    handle, and thread it through a ReaderT/ WebSiteM


#if SERVER
import Happstack.Server
#endif
-- import Common
-- import Sortable
import WebModule.Logger as Logger (log')
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.ModuleScopeURL
import Control.Monad.Trans
import System.FilePath ((</>))
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy as BSL (ByteString)
import Data.Text as Text (Text, pack)
import GHC.Generics
#if CLIENT
import qualified JavaScript.JQuery as JQuery (on, Event, EventType, HandlerSettings, JQuery, AjaxSettings, AjaxResult, ajax)
#endif

default (Text.Text)

baseurl:: ModuleScopeURL
baseurl = $(moduleScopeURL "")

--basepath :: FilePath
--basepath = moduleScopeURLtoFilePath baseurl


data Foo = Foo deriving (Show, Generic, Eq)

data AJAXBindings a = AJAXBindings {
#if CLIENT
  ajax :: Text -> [(Text,Text)] -> JQuery.AjaxSettings -> IO JQuery.AjaxResult
#endif
  }

data AJAXType a = AJAXType { datatypeName' :: String
                           , encodeJSON' :: a -> BS.ByteString
                           , decodeJSON' :: BSL.ByteString -> Maybe a
                           }

ajaxBindingsGen :: AJAXType a -> AJAXBindings a
ajaxBindingsGen _ajt = AJAXBindings {
#if CLIENT
  ajax = JQuery.ajax
#endif
  }

-- require jQuery
-- ajaxModuleGen :: (Happstack m, Show a, m ~ IO) => AJAXType a -> WebSiteM m (AJAXBindings a)

#if CLIENT
#define SERVERONLY(x) Nothing
#else
#define SERVERONLY(x) (Just (x))
#endif
#if CLIENT
#define CLIENTONLY(x) (Just (x))
#else
#define CLIENTONLY(x) Nothing
#endif

#if CLIENT
ajaxModuleGen :: (Show a, Show b, Monad m) => AJAXType a -> m b -> WebSiteM m (AJAXBindings a)
ajaxModuleGen ajt _ = return (ajaxBindingsGen ajt)
#else
ajaxModuleGen :: (Show a, ToMessage b, Monad m, m ~ IO) => AJAXType a -> m b -> WebSiteM m (AJAXBindings a)
ajaxModuleGen ajt m = wimport ws (ajaxBindingsGen ajt)
  where ws :: WebSite
        -- TODO this should be jQuery, with a new base URL.
        ws = mzeroWebSite { serverpart = ajaxHandler (datatypeName' ajt) (decodeJSON' ajt) m
                          , headers = []
                          , bodies = []
                          , baseURL = [baseurl]
                          }
#endif

ajaxURL :: String
ajaxURL = "/ajax"

_ajaxURLT :: Text.Text
_ajaxURLT = Text.pack ajaxURL

#if SERVER
ajaxHandler :: (ToMessage b, Show a, Monad m, MonadIO m, Functor m, m ~ IO) => String -> (BSL.ByteString -> Maybe a) -> IO b -> ServerPartT IO Response
ajaxHandler messageKey dcd' m = dirs ajaxURL $ do
          rq <- askRq 
          liftIO $ Logger.log' $ show rq
          let rqpath = foldr (</>) "" $ rqPaths rq
          liftIO $ Logger.log' $ rqpath
          decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
          msgValue <- lookBS $ messageKey
          let msg = dcd' msgValue
          x <- liftIO m
          ok (toResponse x)
#endif

-- ajaxHandlersave :: forall (t :: * -> (* -> *) -> * -> *) d f a m. (Datatype d, Generic a, Rep a ~ t d f, Happstack m, MonadIO m, Show a, FromJSON a) => (BSL.ByteString -> Maybe a) -> ServerPartT m Response
-- ajaxHandlersave decode' = lift $ dirs ajaxURL $ h
--   where h = do
--           rq <- askRq 
--           Logger.log' $ show rq
--           let rqpath = foldr (</>) "" $ rqPaths rq
--           Logger.log' $ rqpath
--           decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
--           msgValue <- lookBS $ gname (Proxy :: Proxy a)
--           let msg :: Maybe a = decode msgValue
--           ok $ toResponse "life model decoy"



