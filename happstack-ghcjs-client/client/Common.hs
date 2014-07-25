{-# LANGUAGE DeriveGeneric #-}
module Common where
import Data.Aeson
import GHC.Generics


data MarshalMe = MarshalMe { i :: Int, s :: String } deriving (Eq, Generic, Read, Show)

-- This will work when ghcjs gets upgraded.
-- deriveJSON defaultOptions ''MarshalMe

instance FromJSON MarshalMe
instance ToJSON MarshalMe

foo = Right mm == (eitherDecode $ encode mm)
  where mm = MarshalMe 1 "s"
