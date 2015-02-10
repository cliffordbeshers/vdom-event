{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module WebModule.AJAXModule (ajaxModuleGen, AJAXBindings(..), AJAXType(..)) where

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

default (Text.Text)

baseurl:: ModuleScopeURL
baseurl = $(moduleScopeURL "")

--basepath :: FilePath
--basepath = moduleScopeURLtoFilePath baseurl


data Foo = Foo deriving (Show, Generic, Eq)

data AJAXBindings a = AJAXBindings

data AJAXType a = AJAXType { datatypeName' :: String
                           , encodeJSON' :: a -> BS.ByteString
                           , decodeJSON' :: BSL.ByteString -> Maybe a
                           }

ajaxBindingsGen :: AJAXType a -> AJAXBindings a
ajaxBindingsGen _ajt = AJAXBindings

-- require jQuery
-- ajaxModuleGen :: (Happstack m, Show a, m ~ IO) => AJAXType a -> WebSiteM m (AJAXBindings a)

#if CLIENT
ajaxModuleGen :: (Monad m) => AJAXType a -> WebSiteM m (AJAXBindings a)
ajaxModuleGen ajt = return (ajaxBindingsGen ajt)
#else
ajaxModuleGen :: (Show a, Happstack m, m ~ IO) => AJAXType a -> WebSiteM m (AJAXBindings a)
ajaxModuleGen ajt = wimport ws (ajaxBindingsGen ajt)
  where ws :: WebSite
        -- TODO this should be jQuery, with a new base URL.
        ws = mzeroWebSite { serverpart = ajaxHandler (datatypeName' ajt) (decodeJSON' ajt)
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
ajaxHandler :: (Happstack m, Show a) => String -> (BSL.ByteString -> Maybe a) -> ServerPartT m Response
ajaxHandler messageKey dcd' = lift $ dirs ajaxURL $ h
  where h = do
          rq <- askRq 
          Logger.log' $ show rq
          let rqpath = foldr (</>) "" $ rqPaths rq
          Logger.log' $ rqpath
          decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
          msgValue <- lookBS $ messageKey
          let msg = dcd' msgValue
          ok $ toResponse $ "life model decoy " ++ show msg
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

