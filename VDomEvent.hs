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
    where hnd e = do
            print ("click"::Text,ident)
            putMVar redrawChannel  update

render :: Application State
render = do
  liftIO $ print "render onesec" >> onesec
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

  loadInlineBlock "div" -- custom css
  loadBootstrap -- insert dom elements for bootstrap
  root <- mkRoot -- The DOM element at the root of all updates
  eventLoop render (State 0) root

  return ()


data EventLoopState a = ELS { dom :: VNode -- The virtual dom for a frame
                            , detachers :: [IO ()] -- thunks that remove event handlers
                            , supplyList :: [Integer] -- Node id supply list
                            , appState :: a
                            }

printELS :: Show s => EventLoopState s -> IO ()
printELS els = do
  putStrLn "ELS {"
  print (s "dom", s "<no info>")
  print (s "length detachers", length (detachers els))
  print (s "take 3 $ supplyList", take 3 (supplyList els))
  print (s "appState", appState els)
  putStrLn "}"
    where s :: Text -> Text
          s = id

-- The outer IO creates an event handler, the returned IO cleans it up
eventLoop :: Show s => Application s -> s -> DOMNode -> IO ThreadId
eventLoop application s0 root =
  forkIO $ do
    redrawChannel <- newMVar id
    let els = ELS { dom = emptyDiv, detachers = [], supplyList = [0..], appState = s0 }
    forever' els redrawChannel
      where      
        forever' els redrawChannel = do
            print ("eventLoop", "takeMVar redrawChannel")
            -- all event handlers write update :: (s -> s) to this mvar.
            -- wait for a change.
            update <- takeMVar redrawChannel
            print ("eventLoop", "got an update")
            printELS els
            ((r1, state1, attachers1), supply1) <-
              runSupplyT (runRWST application redrawChannel (update (appState els))) (supplyList els)
            -- Figure out the changes from before event to after.
            let p' = diff (dom els) r1
            -- Detach all the handlers from the last loop.
            sequence (detachers els)
            -- Update the page with the DOM changes
            patch root p'
            -- Attach all the new handlers, save the detachers for the next time around.
            detachers1 <- sequence attachers1
            print "forever' debugging pause" >> onesec
            forever' (ELS { dom = r1, detachers = detachers1, supplyList = supply1, appState = state1 }) redrawChannel

-- eventLoop' :: Application State -> State -> IO ThreadId
-- eventLoop' application s0 =
--   forkIO $ do
--       let supply = [0..] :: [Integer]
--       root <- mkRoot
--       loadInlineBlock "div"
--       loadBootstrap
--       lastdraw <- newMVar (emptyDiv, [], supply, s0)
--       redrawChannel <- newMVar id
--       forever $ do
--         print ("eventLoop", "takeMVar redrawChannel")
--         update <- takeMVar redrawChannel
--         print ("eventLoop", "tookMVar redrawChannel")
--         (r0,detachers0,supply0,state0) <- takeMVar lastdraw
--         print ("eventLoop", "tookMVar lastDraw", state0, update state0, take 3 $ supply0)
--         ((r1, state1, attachers1), supply1) <-
--               runSupplyT (runRWST application redrawChannel (update state0)) supply0
--         let p' = diff r0 r1
--         sequence detachers0
--         patch root p'
--         detachers1 <- sequence attachers1
--         putMVar lastdraw (r1, detachers1, supply1, state1)
--         print "onesec" >> onesec
