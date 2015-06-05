module Documentation where

class Documentation a where
  label :: a -> Text
  documentation :: a -> Text
