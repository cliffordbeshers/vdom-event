module Documentation where

import Data.Text (Text)

class Documentation a where
  label :: a -> Text
  documentation :: a -> Text

mkAllTabs :: (Bounded a, Enum a, Documentation a) => [(Text, a)]
mkAllTabs = zip labels values
  where labels = map label values
        values = [ minBound .. maxBound ]
