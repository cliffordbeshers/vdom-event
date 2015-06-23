{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

{-
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}

module Main where

import           Prelude hiding (div)

import           Control.Concurrent
import           Control.Concurrent.MVar

import           Data.Default
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import           System.IO

import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Types
import           Bootstrap
import           UtilQQ
import           JavaScript.JQuery (click, select)


data State = State Int deriving Show

mkState :: Int -> State
mkState = State

step :: State -> State
step (State s) = State (s+1)

render :: State -> VNode
render s = div_ (cls "state") $$ [button', numDiv]
    where
      button' = button_ (id_ "1") $$ [textt "Hello"]
      numDiv   = div_ (cls "numeric") $$ [memo $ textDiv s]

animate :: DOMNode -> VNode -> MVar State -> (State -> IO (IO ())) -> IO ()
animate n r q ev = do
  print "animate"
  s <- takeMVar q
  let r' = render s
      p  = diff r r'
  atAnimationFrame (patch n p >> ev s >> animate n r' q ev)

redraw :: DOMNode -> Patch -> IO ()
redraw node p = p `seq` atAnimationFrame (patch node p)


main :: IO ()
main = do
  root <- mkRoot
  q <- newMVar $ mkState 0

  let ev state = do
        print ("ev", state)
        select (byId "1") >>= click (\_ -> print ("click", step state) >> putMVar q (step state)) def
  animate root emptyDiv q ev


