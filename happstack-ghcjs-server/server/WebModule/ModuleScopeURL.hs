{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

#if CLIENT
module WebModule.ModuleScopeURL where
#else
module WebModule.ModuleScopeURL (ModuleScopeURL, moduleScopeURL, moduleScopeURLtoURI
                                , moduleScopeURLtoFilePath
                                , moduleScopeAppend
                                , URI
                                ) where
#endif

#if SERVER

import Data.Maybe (fromJust)
import Language.Haskell.TH (Exp, Loc(loc_module), location, Q)
import Language.Haskell.TH.Lift (deriveLift, Lift(lift))
import Network.URI (parseRelativeReference, URI)
import System.FilePath ((</>))
import Text.Blaze.Html5 as H (ToValue(..))

data ModuleScopeURL = ModuleScopeURL String FilePath deriving (Eq,Show)

$(deriveLift ''ModuleScopeURL)

-- should probably be a functor, but it's a fixed type.
moduleScopeAppend :: ModuleScopeURL -> FilePath -> ModuleScopeURL
moduleScopeAppend (ModuleScopeURL s fp1) fp2 = ModuleScopeURL s (fp1 </> fp2)

moduleScopeURL :: FilePath -> Q Exp -- ModuleScopeURL
moduleScopeURL fp = do
  loc <- location
  lift $ ModuleScopeURL (loc_module loc) fp
  
moduleScopeURLtoURI :: ModuleScopeURL -> URI
moduleScopeURLtoURI (ModuleScopeURL s fp) =
  fromJust . parseRelativeReference $ moduleNameToPath s </> fp
  
moduleScopeURLtoFilePath :: ModuleScopeURL -> FilePath
moduleScopeURLtoFilePath (ModuleScopeURL s fp) =
  moduleNameToPath s </> fp

-- dot is a legal character, why not leave it?
moduleNameToPath :: String -> FilePath
moduleNameToPath = id -- map dotToSlash
--  where dotToSlash c = if c == '.' then '_' else c

instance ToValue ModuleScopeURL where
  toValue  = toValue . show . moduleScopeURLtoURI
#endif
