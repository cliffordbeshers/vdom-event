import Control.Monad.Writer

type A a = Writer [Int] a
type Z = Writer [Int] ()


a :: A Int
a = do
  return 1
  return 2
