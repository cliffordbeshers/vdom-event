{-# LANGUAGE DeriveGeneric #-}

----------------------------------------------------------------
-- WARNING: THIS FILE IS SHARED BETWEEN TWO PACKAGES
-- HAPPSTACK-GHCJS-CLIENT and HAPPSTACK-GHCJS-SERVER
-- CURRENTLY THIS IS DONE BY KEEPING A COPY IN SYNC IN EACH DIRECTORY.
-- NEEDS AN AUTOMATIC MECHANISM TO EXPORT FROM THE CLIENT TO THE SERVER DURING BUILD.

module Common where
import Data.Aeson
import GHC.Generics
import Data.Text as T (Text, unpack, pack)


ajaxURL :: String
ajaxURL = "/ajax"

ajaxURLT :: T.Text 
ajaxURLT = T.pack ajaxURL


data MarshalMe = MarshalMe { i :: Int, s :: String } deriving (Eq, Generic, Read, Show)

-- This will work when ghcjs gets upgraded.
-- deriveJSON defaultOptions ''MarshalMe

instance FromJSON MarshalMe
instance ToJSON MarshalMe

messageKey :: String
messageKey = datatypeName $ from (MarshalMe 1 "2")

foo = Right mm == (eitherDecode $ encode mm)
  where mm = MarshalMe 1 "s"
