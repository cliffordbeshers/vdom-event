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
render s = div_ (cls "state") $$ [button', button2', numDiv]
    where
      button' = button_ (id_ "1") $$ [textt "Hello"]
      button2' = button_ (id_ "2") $$ [textt "wakeup"]
      numDiv   = div_ (cls "numeric") $$ [memo $ textDiv s]

animate :: DOMNode -> VNode -> MVar State -> (State -> IO (IO ())) -> IO ()
animate n r q ev = do
  print "animate waiting"
  s <- takeMVar q
  print "animate woke"
  let r' = render s
      p  = diff r r'
  atAnimationFrame (print "inside atAnimationFrame" >> patch n p >> ev s >> animate n r' q ev)
  -- s `seq` redraw n p >> ev s >> threadDelay 20000 >> animate n r' q ev -- for async calculation, sync repaint

redraw :: DOMNode -> Patch -> IO ()
redraw node p = p `seq` atAnimationFrame (patch node p)


main :: IO ()
main = do
  root <- mkRoot
  q <- newMVar $ mkState 0

  let ev state = do
        print ("ev", state)
        select (byId "1") >>= click (\_ -> print ("click", step state) >> putMVar q (step state)) def
        select (byId "2") >>= click (\_ -> do print "wakeup" >> takeMVar q >>= (\s-> print ("wakeup",state))) def
  animate root emptyDiv q ev


