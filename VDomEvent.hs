{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Writer
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

render :: State -> Writer [IO (IO ())] VNode
render (State i) = do tell $ [print "attach" >> return (print "detach") ]
                      dom i
  where dom i = if i < 1 then return $ button_ noProps $$ [textt (textshow i)]
                else do left <- render (State (i-1))
                        right <- render (State (i-1))
                        return $ div_ inlineBlock $$ [ button_ noProps $$ [textt (textshow i)], left, right ]

main = do
  b <- select "<button>Click me.</button>"
  select "body" >>= appendJQuery b
  state <- newMVar (State 0)
  redraw <- newEmptyMVar

  click (\e -> do { m <- fmap mkMsg (update  state); print ("click",m) ;  putMVar redraw m }) def b

  eventLoop render redraw

  return ()


-- The outer IO creates an event handler, the returned IO cleans it up
type MkHandler = IO (IO ())

eventLoop :: (State -> Writer [MkHandler] VNode) -> MVar State -> IO ThreadId
eventLoop application redrawChannel =
  forkIO $ do
      top <- mkRoot
      loadInlineBlock "div"
      loadBootstrap
      lastdraw <- newMVar (emptyDiv, [])
      forever $ do
        msg <- takeMVar redrawChannel
        print ("fork",msg)
        (r,detach) <- takeMVar lastdraw
        let (r', attach') = runWriter $ render msg
        let p' = diff r r'
        sequence detach
        patch top p'
        detach' <- sequence attach'
        putMVar lastdraw (r', detach')
        
        
