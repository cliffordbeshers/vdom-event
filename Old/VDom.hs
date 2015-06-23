{-# LANGUAGE OverloadedStrings #-}

{-
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}
module Main where

import Prelude hiding (div)


import Control.Monad (void)
-- import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Data.Default (def)
-- import System.IO
import Data.Text (Text)

import JavaScript.JQuery as JQuery (click, find, select)

import GHCJS.VDOM as VDOM (DOMNode, VNode, Patch,
                           div, noProps, mkChildren, memo,
                           diff, patch, emptyDiv
                          )
import UtilQQ
import Bootstrap
default (Text)

data State = State { words :: [Int] } deriving Show

mkState :: Int -> State
mkState n = State $ [1..n]

step :: State -> State
-- step (State ws) = State $ cycle1 ws --(map (+1) ws) -- (cycle ws)
step (State ws) = State $ fib $ cycle1 ws --(map (+1) ws) -- (cycle ws)

fib :: [Int] -> [Int]
fib [] = []
fib (x:[]) = [x]
fib (x:y:ys) = [x+y,x,y] ++ ys

cycle1 :: [a] -> [a]
cycle1 [] = []
cycle1 (x:xs) = xs ++ [x]

render :: State -> VNode
render (State ws) = div noProps . mkChildren . map renderWord $ ws

renderWord :: Int -> VNode
renderWord = memo textDiv

-- animate :: DOMNode -> VNode -> State -> IO ()
-- animate n r s =
--   let s' = step s
--       r' = render s'
--       p  = diff r r'
--   in  s' `seq` redraw n p >> threadDelay 1000000 >> animate n r' s' -- for async calculation, sync repaint
--   in atAnimationFrame (patch n p >> animate n r' s' ) -- sync all

animate' :: DOMNode -> VNode -> MVar State -> MVar State -> IO (IO ()) -> IO ()
animate' n r inState outState evs = do
  s <- takeMVar inState
  print ("animate'" :: String, s)
  let r' = render s
  let p  = diff r r'
  s `seq` redraw n p (evs >> putMVar outState s) >> animate' n r' inState outState evs -- for async calculation, sync repaint
--   in atAnimationFrame (patch n p >> animate n r' s' ) -- sync all

redraw :: DOMNode -> Patch -> IO a -> IO ()
redraw node p evs = p `seq` atAnimationFrame (patch node p >> void evs)

allclicks :: MVar State -> MVar State -> IO (IO ())
allclicks inState outState = do
  print ("allclicks" :: Text)
  let action _ = void $ do
        s <- takeMVar outState
        print ("allclicks" :: Text,s)
        putMVar inState (step s)
  select "body" >>= find ".word" >>= (\s -> print "clicking" >> click action def s)


main :: IO ()
main = do
  _ <- loadBootstrap
  _ <- inlineCSS "h3 { display: inline-block; }"
  root <- mkRoot
  let s = mkState 1
  inState <- newMVar s
  outState <- newEmptyMVar
  let r' = render s
  let p  = diff emptyDiv r'
  redraw root p (allclicks inState outState)
  select ".word" >>= (\s -> print "clicking" >> click (\_-> print "click") def s)
  select "#escape" >>= (\s -> print "fooing" >> click (\_-> allclicks inState outState >> print "foo") def s)
  return ()
  -- _gc <- allclicks inState outState
  --  animate' root emptyDiv inState outState (allclicks inState outState)


yield :: MVar State -> State -> IO ()
yield = putMVar

block :: MVar State -> IO State
block = takeMVar
