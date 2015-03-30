{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
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

render :: State -> VNode
render (State i) =
  if i < 1 then button_ noProps $$ [textt (textshow i)]
  else div_ inlineBlock $$ [ button_ noProps $$ [textt (textshow i)], render (State (i-1)), render (State (i-1)) ]

main = do
  b <- select "<button>Click me.</button>"
  select "body" >>= appendJQuery b
  state <- newMVar (State 0)
  redraw <- newEmptyMVar

  click (\e -> do { m <- fmap mkMsg (update  state); print ("click",m) ;  putMVar redraw m }) def b

  zippy render redraw

  return ()


zippy render redraw =
  forkIO $ do
      top <- mkRoot
      loadInlineBlock "div"
      loadBootstrap
      lastdraw <- newMVar emptyDiv
      forever $ do
        msg <- takeMVar redraw
        print ("fork",msg)
        r <- takeMVar lastdraw
        let r' = render msg
        putMVar lastdraw r'
        let p' = diff r r'
        patch top p'
