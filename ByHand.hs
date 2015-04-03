{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Data.Default
import Data.Monoid
import Data.Text (Text, pack)
import Data.Time
import GHCJS.Foreign
import GHCJS.Foreign.QQ
import GHCJS.VDOM
import GHCJS.VDOM.QQ
import JavaScript.JQuery
import ControlMonadSupplyExcept
import Bootstrap
import UtilQQ

default (Text)

type MkHandler = IO (IO ())

onesec :: IO ()
onesec = threadDelay 1000000

-- type Application state = RWST (MVar (state -> state)) [MkHandler] state (Supply Integer) VNode
type ApplicationT m state = RWST (MVar (state -> state)) [MkHandler] state (SupplyT Integer m)  VNode
type Application state = ApplicationT IO state

-- update :: MVar State -> IO State
-- update state = do
--   State i <- takeMVar state
--   putMVar state (State (i+1))
--   return $ State (i+1)

mkMsg = id

data State = State Int deriving (Show)

inlineBlock :: Properties
inlineBlock = cls "ib"

loadInlineBlock :: Text -> IO ()
loadInlineBlock tag = void $ inlineCSS (tag <> " { display: inline-block; }")


click' :: (MVar (State-> State)) -> Text -> (State -> State) -> IO (IO ())
click' redrawChannel ident update = do
  select (byId ident) >>= click hnd def
    where hnd _e = putMVar redrawChannel update

succ' :: State -> State
succ' (State i) = State (succ i)

render :: MVar (State -> State) -> State -> IO (VNode, IO (IO ()))
render redrawChannel (State i) = do
  let ident = textshow i
  let scene = div_ noProps $$ [ button_ (id_ ident) $$ [textt "Click to update" ]
                              , textt ident ]
  let attacher = click' redrawChannel ident succ'
  return (scene, attacher)
  
          
main = do
  loadInlineBlock "div" -- custom css
  loadBootstrap -- insert dom elements for bootstrap
  root <- mkRoot -- The DOM element at the root of all updates
  redrawChannel <- newMVar id
  forkIO $ eventLoop root render (State 0) redrawChannel (emptyDiv, return ())

eventLoop :: DOMNode -> (MVar (State -> State) -> State -> IO (VNode, IO (IO ()))) -> State -> MVar (State -> State) -> (VNode, IO ()) -> IO ()
eventLoop root render state redrawChannel (olddom, olddetacher) = do
  update <- takeMVar redrawChannel
  let newstate = update state
  (newdom, newattacher) <- render redrawChannel newstate
  let p' = diff olddom newdom
  olddetacher
  patch root p'
  newdetacher <- newattacher
  eventLoop root render newstate redrawChannel (newdom, newdetacher)
  
  


  

