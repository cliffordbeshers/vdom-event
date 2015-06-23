{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

{-
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}

module Main where

import           Prelude hiding (div)

import           Control.Concurrent

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import           System.IO

import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Types
import Bootstrap
import UtilQQ


data State = State Int deriving Show

mkState :: Int -> State
mkState = State

step :: State -> State
step (State s) = State (s+1)

($$) :: (Children -> VNode) -> [VNode] -> VNode
node $$ cs = node (mkChildren cs)

render :: State -> VNode
render s = div_ (cls "state") [ch|button', numDiv|]
    where
      button' = button_ noProps $$ [textt "Hello"]
      numDiv   = div_ (cls "numeric") $ mkChildren [memo $ textDiv s]

animate :: DOMNode -> VNode -> State -> IO ()
animate n r s =
  let s' = step s
      r' = render s'
      p  = diff r r'
--  in  s' `seq` redraw n p >> threadDelay 20000 >> animate n r' s' -- for async calculation, sync repaint
  in atAnimationFrame (patch n p >> animate n r' s') -- sync all

redraw :: DOMNode -> Patch -> IO ()
redraw node p = p `seq` atAnimationFrame (patch node p)

main :: IO ()
main = do
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  let s = mkState 0

  animate root emptyDiv s


