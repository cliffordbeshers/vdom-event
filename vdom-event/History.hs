{-# LANGUAGE RankNTypes, TypeFamilies #-}
module History where

import Control.Lens

-- Some of these should clearly be type classes/families.

data Meta u t a = Meta { user :: u, timestamp :: t, notmeta :: a }

-- Unimplemented, see Yoko on hackage.
-- A union of all the leaf types of u, i.e., anything a (simple?) lens can focus on.
-- Or perhaps a union of all types v for Biplate u v operations.
data Universe u = Universe u

-- type ULens s = Lens' s (Universe s)
type ULens s = Maybe s
type HistoryEvent u t v = Meta u t (ULens v)

data History u t v = History [HistoryEvent u t v]



-- Shouldn't this just be fmap on a stack?

push :: History u t v -> HistoryEvent u t v -> History u t v
push (History hs) h = History (h : hs)

pop :: History u t v -> (History u t v, Maybe (HistoryEvent u t v))
pop (History []) = (History [], Nothing)
pop (History (h:hs)) = (History hs, Just h)

peek :: History u t v -> (History u t v, Maybe (HistoryEvent u t v))
peek (History []) = (History [], Nothing)
peek (History hs@(h:_)) = (History hs, Just h)
