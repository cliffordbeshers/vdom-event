{-# LANGUAGE FlexibleInstances, QuasiQuotes, TypeFamilies, DeriveDataTypeable, TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances #-}
module AppraisalScribe.JSGetSet where

import Language.Javascript.JMacro
-- import qualified Data.Text as T
-- import Data.Typeable
-- import Data.Data
-- import Database

class JSGetSet a where
    get :: a -> JExpr
    set :: a -> JExpr

{- - shouldn't this be
class JSGetSet a where
    get :: String -> JExpr -> JExpr
    set :: String -> JExpr -> JExpr -> JExpr

this = [jmacroE|this|]
get "s" this
-}


--What is the argument to get/set?

instance JSGetSet String where
    get s = [jmacroE|`(s)`.val()|]
    set s = [jmacroE|`(s)`.val(v)|]

jsUndefined :: String -> JExpr
jsUndefined message = [jmacroE|alert(`("jsUndefined: " ++ message)`)|]
