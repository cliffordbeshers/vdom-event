{-# LANGUAGE TemplateHaskell #-}
module Test.ModuleScope where

import Network.URI
import ModuleScopeURL

x :: ModuleScopeURL
x = $(moduleScopeURL "x")