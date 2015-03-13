
import ControlMonadSupply

type GM = Supply Int 

data W = W Int deriving Show

a :: GM W
a = do
  i <- supply
  return (W i)
  
b :: GM W
b = do
  i <- supply
  return (W i)
  


main = print $ evalSupply (a >> b >> a >> b) [0..]

