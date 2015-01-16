{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable


foo :: Show a => [a] -> [String]
foo = map show