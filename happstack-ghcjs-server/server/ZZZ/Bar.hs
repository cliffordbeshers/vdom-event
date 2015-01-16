
import Control.Monad

foo,bar :: String
[foo,bar] = ["",""]

f = putStrLn . (\_ -> getLine)
  where (.) = (>=>)
