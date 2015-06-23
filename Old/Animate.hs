{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

{-
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}
module Main where

import Prelude hiding (div)


import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Data.Default (def)
import System.IO

import GHCJS.Foreign
import GHCJS.Foreign.QQ
import JavaScript.JQuery as JQuery (click, find, select)
import GHCJS.Types
import GHCJS.VDOM as VDOM (DOMNode, Patch, Properties, VNode, Children,
                           div, noProps, mkChildren, memo, text,
                           diff, patch, emptyDiv, vnode
                          )
import GHCJS.VDOM.QQ
import UtilQQ
import Bootstrap

data State = State { words :: [Int] }

mkState :: Int -> State
mkState n = State $ [10000..10000+n]

step :: State -> State
step (State ws) = State $ cycle1 ws --(map (+1) ws) -- (cycle ws)

cycle1 :: [a] -> [a]
cycle1 [] = []
cycle1 (x:xs) = xs ++ [x]

cls :: JSString -> Properties
cls name = [pr| className: name |]

render :: State -> VNode
render (State ws) = div noProps . mkChildren . map renderWord $ ws

renderWord :: Int -> VNode
renderWord = memo textDiv

textDiv :: Show a => a -> VNode
textDiv x = button (cls "word text-primary") [ch|c|]
  where
    c = text . toJSString . show $ x

{-# INLINE h3 #-}
h3 :: Properties -> Children -> VNode
h3 = vnode "h3"
button :: Properties -> Children -> VNode
button = vnode "button"


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
  let r' = render s
  let p  = diff r r'
  s `seq` redraw n p evs >> putMVar outState s >> animate' n r' inState outState evs -- for async calculation, sync repaint
--   in atAnimationFrame (patch n p >> animate n r' s' ) -- sync all

redraw :: DOMNode -> Patch -> IO (IO ()) -> IO ()
redraw node p evs = p `seq` atAnimationFrame (patch node p >> void evs)

foo :: DOMNode -> IO ()
foo _ = return ()

allclicks :: MVar State -> MVar State -> IO (IO ())
allclicks inState outState = do
  print "allclicks xxx"
  let action _ = void $ do
        print ("Hello, world!" :: String)
        s <- takeMVar outState
        putMVar inState (step s)
  select "body" >>= find ".word" >>= (\s -> print "clicking" >> click action def s)


main :: IO ()
main = do
  _ <- loadBootstrap
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  _ <- inlineCSS "h3 { display: inline-block; }"
  let s = mkState 1
  inState <- newMVar s
  outState <- newEmptyMVar
  let r' = render s
  let p  = diff emptyDiv r'
  redraw root p (allclicks inState outState)
  select ".word" >>= (\s -> print "clicking" >> click (\_-> print "click") def s)
  select "#foo" >>= (\s -> print "fooing" >> click (\_-> allclicks inState outState >> print "foo") def s)
  return ()
  -- _gc <- allclicks inState outState
   --  animate' root emptyDiv inState outState (allclicks inState outState)
