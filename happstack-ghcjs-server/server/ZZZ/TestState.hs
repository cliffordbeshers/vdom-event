module TestState where

import Control.Monad.State


type S = State Int ()


x :: S
x = modify (+17)

y :: S
y = modify (+2004)

z = do
  x
  y
