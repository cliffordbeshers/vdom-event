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

type Application state = RWST (MVar (state -> state)) [MkHandler] state (Supply Integer) VNode
type ApplicationT m state = RWST (MVar state) [MkHandler] state m VNode

update :: MVar State -> IO State
update state = do
  State i <- takeMVar state
  putMVar state (State (i+1))
  return $ State (i+1)

mkMsg = id

data State = State Int deriving (Show)

inlineBlock :: Properties
inlineBlock = cls "ib"

loadInlineBlock :: Text -> IO ()
loadInlineBlock tag = void $ inlineCSS (tag <> " { display: inline-block; }")


click' :: (MVar (State-> State)) -> Text -> (State -> State) -> IO (IO ())
click' redrawChannel ident update = do
  select (byId ident) >>= click hnd def
    where hnd e = do
            print ("click"::Text,ident)
            putMVar redrawChannel  update

render :: Application State
render = do
  State i <- get
  liftIO $ print ("render", State i)
  ident <- fmap (textshow) (lift supply)
  redrawChannel <- ask
  tell $ [print "tell click" >> click' redrawChannel ident succ']
  dom i ident
    where dom i ident =
                  if i < 1 then return $ button_ (id_ ident) $$ [textt (textshow i)]
                  else do left <- recurse render
                          right <- recurse render
                          return $ div_ inlineBlock $$ [ button_ (id_ ident) $$ [textt (textshow i)], left, right ]
          recurse :: Application State -> Application State
          recurse = withRWST (\r s -> (r, succ' s))
          succ' (State i) = State $ succ i
          

-- Add a Value Supply monad.
-- Attach an id to each node, or perhaps to each click target.
-- run click byId  

main = do
  print "No button"
  b <- select "<button>Click me.</button>"
  select "body" >>= appendJQuery b
--   click (\e -> do { m <- fmap mkMsg (update  state); print ("click",m) ;  putMVar redraw m }) def b

  eventLoop render (State 0)

  return ()


-- The outer IO creates an event handler, the returned IO cleans it up
eventLoop :: Application State -> State -> IO ThreadId
eventLoop application s0 =
  forkIO $ do
      let supply = [0..] :: [Integer]
      top <- mkRoot
      loadInlineBlock "div"
      loadBootstrap
      lastdraw <- newMVar (emptyDiv, [], supply, s0)
      redrawChannel <- newMVar id
      forever $ do
        print ("eventLoop", "takeMVar redrawChannel")
        update <- takeMVar redrawChannel
        print ("eventLoop", "tookMVar redrawChannel")
        (r0,detachers0,supply0,state0) <- takeMVar lastdraw
        print ("eventLoop", "tookMVar lastDraw", state0, update state0, take 3 $ supply0)
        let ((r1, state1, attachers1), supply1) =
              runSupply (runRWST application redrawChannel (update state0)) supply0
        let p' = diff r0 r1
        sequence detachers0
        patch top p'
        detachers1 <- sequence attachers1
        putMVar lastdraw (r1, detachers1, supply1, state1)
        
        
