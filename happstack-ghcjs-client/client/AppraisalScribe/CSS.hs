{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS -Wall #-}

module AppraisalScribe.CSS ( CSS(..)
           , DimensionUnit(..)
           , Dimension(..)
           , Color
           , cssdim
           , AppraisalScribe.CSS.em
           , formatCSS
           , inlineCSS
           , tempcss
           , unitTxt
           , unitStr
           )
    where

import Prelude hiding (map, undefined)
import qualified Prelude as P (map)
import Data.Monoid ((<>))
import Data.Typeable
import Data.Data
import Data.List (intercalate, intersperse)
import Data.Text as Text (Text, concat, pack)
-- import Network.URI
import Lucid
import AppraisalScribe.Reindeer

default (Text)

-- ToDo YAK change to Tagged
-- YAK? is type alias best was to simplify Tagged?
newtype CSS = CSS { unCSS :: Text } deriving (Eq, Ord, Read, Show, Typeable, Data)

instance ToHtml CSS where
  toHtml = toHtml . unCSS
  toHtmlRaw = toHtmlRaw . unCSS

type Color = String

-- data DimensionTag = DimensionTag
data DimensionUnit = Em | Inch | Px deriving (Eq, Ord, Read, Show)

data Dimension = Dimension Float DimensionUnit deriving (Eq, Ord, Read, Show)

instance Reindeer Dimension Text where
    reindeer (Dimension f u) = Text.pack (show f) <> unitTxt u

-- type Dimension = Tagged DimensionTag CSS

-- This should be Reindeer
unitStr :: DimensionUnit -> String
unitStr Inch = "in"
unitStr x = show x

unitTxt :: DimensionUnit -> Text
unitTxt Inch = "in"
unitTxt x = Text.pack $ show x

cssdim :: (Show a, Num a) => a -> DimensionUnit -> CSS
cssdim n u = CSS $ Text.pack (show n) <> unitTxt u

-- this is bogus and needs fixing.  Look for a CSS library.
formatCSS :: [(Text, Text)] -> Text
formatCSS = Text.concat . wrap . semis . P.map colon
    where
      wrap xs = ["{"] ++ xs ++ ["}"]
      semis = intersperse "; "
      colon (f,v) = Text.concat [f,": ",v]

em :: Text -> Text
em = (<> "em")

inlineCSS :: [(Text, Text)] -> Text
inlineCSS = Text.concat . semis . P.map colon
    where
      semis = intersperse "; "
      colon (f,v) = Text.concat [f,": ",v]

tempcss :: Html ()
tempcss = inlinecss $ fmt fields
    where inlinecss = style_ [type_ "text/css"]
          fmt fs = intercalate "\n" fs
          fields :: [String]
          fields = [ "table { width: 100% }"
--                   , ".hbox { border-style:solid; border-color: black; border-width: 1 }"
                   , ".vbox { border-style:solid; border-color: #DDDDDD; border-width: 1 }"
                   ]

{-
externalcss :: URI -> Html
externalcss uri = H.link
                  ! HA.rel "stylesheet"
                  ! HA.href (toValue $ show uri)
                  ! HA.type_ "text/css"
                  ! HA.media "all"
-}
