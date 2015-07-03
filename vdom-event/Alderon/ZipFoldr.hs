{-# LANGUAGE ScopedTypeVariables #-}

zip2 :: [a] -> [b] -> [(a,b)]
zip2 xs ys = foldr step done xs ys
  where done :: [b] -> [(a, b)]
        done ys = []
        step :: a -> ([b] -> [(a, b)]) -> [b] -> [(a, b)]
        step x zipsfn []     = []
        step x zipsfn (y:ys) = (x, y) : (zipsfn ys)


