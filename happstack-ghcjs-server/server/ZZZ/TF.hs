{-# LANGUAGE TypeFamilies #-}

type family F a where
  F Int = Bool
  F Char = Bool
  F Double = Float

