{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module ModuleScopeURL (ModuleScopeURL, moduleScopeURL, moduleScopeURLtoURI
                      , moduleScopeURLtoFilePath
                      , URI
                      ) where

import Language.Haskell.TH
import Language.Haskell.TH.Lift
import System.FilePath (FilePath, (</>))
import WithLocation (moduleNameTH)
import Data.Typeable
import Data.Data
import Data.Maybe (fromJust)
import Network.URI

data ModuleScopeURL = ModuleScopeURL String FilePath deriving (Eq,Show)

$(deriveLift ''ModuleScopeURL)

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
