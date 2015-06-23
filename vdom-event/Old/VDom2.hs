{-# LANGUAGE OverloadedStrings #-}
module VDom2 where

import Prelude hiding (div)

import Control.Monad (forever, void)
import Data.Default (def)
import Data.Text
import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Types

import GHCJS.VDOM as VDOM (DOMNode, VNode, Patch,
                           div, noProps, mkChildren, memo,
                           diff, patch, emptyDiv
                          )

import GHCJS.VDOM.QQ
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newMVar, newEmptyMVar)
import JavaScript.JQuery (click, select)
import UtilQQ

default (Text)

data State = State Int

step :: State -> State
step (State s) = State (succ s)

--diff :: VNode -> VNode -> Patch
--diff (VNode a) (VNode b) = Patch (b - a)

-- patch :: DOMNode -> Patch -> IO DOMNode
-- patch (DOMNode n) (Patch d) = return (DOMNode (n+d))



render :: MVar (State -> State) -> State -> (VNode, IO ())
render mvar (State s) = (t, evs)
  where t = div noProps . ch $ [ button (id_ b) . ch $ [textDiv "Succ"]
                               , div noProps . ch $ [ label_ noProps . ch $ [textt "Counter:"]
                                                    , textDiv (textshow s)
                                                    ]
                               ]
        b = textshow 99
        ch = mkChildren
        evs = void $ do
          j <- select (byId b)
          click (\ev -> yield step mvar) def j 

yield :: a -> MVar a -> IO ()
yield a v = putMVar v a

block :: MVar a -> IO a
block = takeMVar



main = do
  let start = State 0
  root <- mkRoot
  delta <- newEmptyMVar
  state <- newMVar start
  
  -- draw the initial state
  
  let (r',evs) = render delta start
  patch root (diff emptyDiv r')
  evs
  forever $ do
    ds <- takeMVar delta
    s <- takeMVar state
    let s' = ds s
    
    putMVar state s'
    patch root (diff s s')
    
      

eloop :: DOMNode -> VNode -> State -> IO ()
eloop n r s = atAnimationFrame (patch' >>  eloop    )
  let s' = step s
      r' = render s'
      p  = diff r r'
--  in  s' `seq` redraw n p >> threadDelay 20000 >> animate n r' s' -- for async calculation, sync repaint
  in atAnimationFrame (patch n p >> animate n r' s') -- sync all
