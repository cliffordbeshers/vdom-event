{-# LANGUAGE QuasiQuotes, StandaloneDeriving, EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module AppraisalScribe.ArrowState where

import Data.Char (toLower)
import Data.Data
import Data.List (intercalate)
import qualified Data.Text as T
import Language.Javascript.JMacro

import AppraisalScribe.Reindeer

data ArrowState = Inactive |
                  Sending |
                  Conflict |
                  Failure |
                  Accepted
                deriving (Eq, Ord, Read, Show, Bounded, Enum)

-- 'cause Enum sucks.
allArrowStates :: [ArrowState]
allArrowStates = [ minBound..maxBound ]

instance Reindeer ArrowState T.Text where
  reindeer x = (T.pack "arrowstate-")  `T.append` (T.toLower . T.pack . show $ x)
instance Reindeer ArrowState String where
  reindeer x = ("arrowstate-")  ++  (map toLower . show $ x)

instance ToJExpr ArrowState where
  toJExpr = toJExpr . (reindeer :: ArrowState -> String)

data Stoplight = Red | Yellow | Green deriving (Eq, Ord, Read, Show, Typeable, Data)

arrowStateImports :: [JStat]
arrowStateImports = [arrowSetState]

arrowSetState :: JStat
arrowSetState =
  [jmacro|function !arrowSetState(sel,state) {
      console.log("arrowSetState (sel,state): (" + sel + "," + state + ")");
      $(sel).removeClass(`(arrowClasses)`).addClass(state);
      }
   |]
  where arrowClasses = intercalate " " $ map f allArrowStates
        f :: ArrowState -> String
        f = reindeer


-- client side states
--  sent request for update
--  waiting for response
--  received update

{-
if local is unchanged, get remote, install

case (old local == new local) of
  True -> (remote, Green)
  False -> case (new local == remote) of
               True -> (new local, Green)
               False -> (remote, Red)

finalize :: Eq a => (a,a) -> a -> (a,Stoplight)
finalize local remote =
  case ((fst local == snd local)

on received update.

two cases
  if local.old == local.new && local.new == remote.new then (Nothing, green)
  if local.old == local.new && remote.new != remote.new then (Nothing, green)
-}


rgetFinal :: Eq a => a -> a -> (a, Stoplight)
rgetFinal local remote =
  (remote, greenRed (local == remote))
  where greenRed b = if b then Green else Red

-- The remote side needs the old value?  Yes, the remote does the set
-- only if the old value matches the DB entry.
-- The response should probably be (Bool, value)
rsetFinal :: Eq a => (a,a) -> a -> (a, Stoplight)
rsetFinal (_old, new) remote =
  -- assume (old != new) otherwise why try a set?
  (remote, greenRed (new == remote))
  where greenRed b = if b then Green else Red




{-
.arrowstate-inactive {
	border: 1em solid transparent;
	border-left-color: #EEE; // transparent;
	height: 0.1em;
	width: 0.1em;
}
.arrowstate-sending {
	border: 1em solid transparent;
	border-left-color: #FF2;
	height: 0.01em;
}
.arrowstate-conflict {
	border: 1em solid transparent;
	border-left-color: #F22;
	height: 0.1em;
}
.arrowstate-accepted {
	border: 1em solid transparent;
	border-left-color: #2F2;
	height: 0.1em;
}
-}
