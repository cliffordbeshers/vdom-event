{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module LucidExample where

import Lucid

lucidExample :: String
lucidExample = show x

x :: Html ()
x = "123 < 456"