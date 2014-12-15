module WebModule.Markable where

-- Existential quantification of distinct types did not work
-- here because I need an Eq instance to nub import lists.

import Text.Blaze.Html5 (ToMarkup(..))
import Network.URI (URI)
import qualified Data.ByteString as B (ByteString)
import WebModule.ModuleScopeURL

-- All the types that can be serialized into the HTML head section.
data WM_Header = 
  WMH_JavaScript ModuleScopeURL |
  WMH_CSS ModuleScopeURL  |
  WMH_Favicon ModuleScopeURL

  deriving (Eq, Show)

-- All the types that can be serialized into the HTML head section.
data WM_Body = 
  WMB_Initialization String  -- Javascript initialization
  deriving (Eq, Show)
