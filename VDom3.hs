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

import           Data.Maybe
import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Types
import           Bootstrap
import           UtilQQ
import           JavaScript.JQuery (click, select)


data State = State !Int deriving Show

mkState :: Int -> State
mkState = State

step :: State -> State
step (State s) = State (s+1)

unstep :: State -> State
unstep (State s) = State (s-1)

render :: State -> VNode
render s = div_ (cls "state") $$ [button', button2', numDiv]
    where
      button' = button_ (id_ "1") $$ [textt "Hello"]
      button2' = button_ (id_ "2") $$ [textt "wakeup"]
      numDiv   = div_ (cls "numeric") $$ [memo $ textDiv s]

animate :: DOMNode -> VNode -> MVar State -> (MVar State -> State -> IO (IO ())) -> IO ()
animate n r q ev = do
  print "animate waiting"
  --  ms <- tryReadMVar q
  s <- takeMVar q
--   let s = fromMaybe (State 0) ms
  print "animate woke"
  let r' = render s
      p  = diff r r'
  atAnimationFrame (print "inside atAnimationFrame" >> patch n p >> ev q s >> animate n r' q ev)
  -- s `seq` redraw n p >> ev q s >> threadDelay 20000 >> animate n r' q ev -- for async calculation, sync repaint

redraw :: DOMNode -> Patch -> IO ()
redraw node p = p `seq` atAnimationFrame (patch node p)

redraw' :: DOMNode -> State -> (State -> IO (IO ())) -> IO ()
redraw' node s ev =
  let p = diff (render (unstep s)) (render s)
  in  p `seq` atAnimationFrame (patch node p >> ev s >> return ())

main :: IO ()
main = do
  root <- mkRoot
  q <- newMVar $ mkState 99
  qq <- newEmptyMVar

  let ev q state = do
        print ("ev", state)
        select (byId "1") >>= click (\_ -> do { s<- return state ; print ("click", s); let s' = step s in s' `seq` (putMVar q s' >> redraw' root s' (ev q)); }) def
--        select (byId "1") >>= click (\_ -> let s' = step state in s' `seq` putMVar q s') def
--        select (byId "2") >>= click (\_ -> do { s <- takeMVar q ; print ("2 <", s) ; putMVar q (step s) ; print ("2 >", step s); }) def
  q `seq` (forkIO $ animate root emptyDiv q ev)
  return ()


