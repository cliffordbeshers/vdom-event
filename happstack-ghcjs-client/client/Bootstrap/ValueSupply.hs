{-# OPTIONS_GHC -Wall #-}
module Bootstrap.ValueSupply (newPrefixSupply, prefixSupply) where

import Data.Foldable
import Data.Supply (modifySupply, newNumSupply, split, split2, split3, Supply, supplyValue)

_example :: IO ()
_example = do
  s0 <- newNumSupply :: IO (Supply Int)
  let (s1,s2,s3) = split3 s0
  let ss = split s2
  print $ supplyValue s0
  print $ supplyValue s1
  print $ supplyValue s2
  print $ take 5 $ map supplyValue ss
  print $ take 5 $ map supplyValue (split s1)
  let s4 = modifySupply s3 (("prefix" ++) . show . supplyValue)
  let (s5,s6) = split2 s4
  print $ supplyValue s4
  print $ take 5 $ map supplyValue (split s5)
  print $ take 5 $ map supplyValue (split s6)

prefixSupply :: Show a => String -> Supply a -> Supply String
prefixSupply prf sup = modifySupply sup ((prf ++) . show . supplyValue)

newPrefixSupply :: String -> IO (Supply String)
newPrefixSupply prefix = do
  s0 <- newNumSupply  :: IO (Supply Int)
  return $ prefixSupply prefix s0
