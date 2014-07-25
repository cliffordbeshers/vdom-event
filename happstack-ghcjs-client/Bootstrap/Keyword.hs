{-# LANGUAGE NoImplicitPrelude #-}
module Clckwrks.AVR3.Bootstrap.Keyword where

import Data.Text (Text)

class Keyword a where
  keyword :: a -> Text

