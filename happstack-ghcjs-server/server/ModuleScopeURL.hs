{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module ModuleScopeURL (ModuleScopeURL, moduleScopeURL) where

import Language.Haskell.TH
import Language.Haskell.TH.Lift
import System.FilePath (FilePath)
import WithLocation (moduleNameTH)
import Data.Typeable
import Data.Data

data ModuleScopeURL = ModuleScopeURL String FilePath deriving (Eq,Show)

$(deriveLift ''ModuleScopeURL)

moduleScopeURL :: FilePath -> Q Exp
moduleScopeURL fp = do
  loc <- location
  lift $ ModuleScopeURL (loc_module loc) fp
  
