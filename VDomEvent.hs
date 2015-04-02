{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
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
import Bootstrap
import UtilQQ

default (Text)

type MkHandler = IO (IO ())

type Application state = RWS (MVar state) [MkHandler] state VNode
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

render :: Application State
render = do
  State i <- get
  tell $ [print "attacher" >> return (print "detacher") ]
  dom i
    where dom i = if i < 1 then return $ button_ noProps $$ [textt (textshow i)]
                  else do left <- recurse render
                          right <- recurse render
                          return $ div_ inlineBlock $$ [ button_ noProps $$ [textt (textshow i)], left, right ]
          recurse :: Application State -> Application State
          recurse = withRWS (\r s -> (r, succ' s))
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
      top <- mkRoot
      loadInlineBlock "div"
      loadBootstrap
      lastdraw <- newMVar (emptyDiv, [])
      redrawChannel <- newMVar s0
      forever $ do
        state <- takeMVar redrawChannel
        print ("fork",state)
        (r0,detachers0) <- takeMVar lastdraw
        let (r1, s1, attachers1) = runRWS application redrawChannel state
        let p' = diff r0 r1
        sequence detachers0
        patch top p'
        detachers1 <- sequence attachers1
        putMVar lastdraw (r1, detachers1)
        
        
