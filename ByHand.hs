{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Data.Default
import Data.Monoid
import Data.Text as Text (Text, pack, reverse)
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

data State = State Int deriving (Show)

inlineBlock :: Properties
inlineBlock = cls "ib"

loadInlineBlock :: Text -> IO ()
loadInlineBlock tag = void $ inlineCSS (tag <> " { display: inline-block; }")


click' :: (MVar (State-> State)) -> Text -> (State -> State) -> IO (IO ())
click' redrawChannel ident update = do
  select (byId ident) >>= click hnd def
    where hnd e = do
            print (("hnd", ident) :: (Text,Text))
            putMVar redrawChannel update

succ' :: State -> State
succ' (State i) = State (1+i)

render :: MVar (State -> State) -> State -> IO (VNode, IO (IO ()))
render redrawChannel (State i) = do
  let ident = textshow i
  let dv1 x = div_ noProps $$ [x]
  let dv xs = div_ noProps $$ xs
--   let bsb id = button_  -- need to be able to merge properties.
  let scene =
        if odd i then
          dv  [ dv1 $ button_ (id_ ident) $$ [textt "Click odd to update" ]
              , dv $ replicate i (textt ident)
              ]
        else
          dv  [ dv1 $ button_ (id_ ident) $$ [textt "Click even to update" ]
              , dv $ replicate (i+i) (textt (Text.reverse ident))
              ]
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
  print ("newstate",newstate)
  (newdom, newattacher) <- render redrawChannel newstate
  let p' = diff olddom newdom
  olddetacher
  patch root p'
  newdetacher <- newattacher
  eventLoop root render newstate redrawChannel (newdom, newdetacher)
  
  


  

