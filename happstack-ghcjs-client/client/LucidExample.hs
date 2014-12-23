{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module LucidExample where

import Control.Applicative
import Data.List
import Data.Text as Text (Text, pack)
import Data.Text.Lazy (toStrict)
import JavaScript.JQuery as JQuery (appendJQuery, click, select, setHtml, setText)
import qualified JavaScript.JQueryExtra as JQuery (hide, show)
import Control.Monad
import Data.Default
import Data.IORef

import Lucid
import Data.Tree
import ZipTree

renderLucid :: Html () -> Text
renderLucid = toStrict . renderText

-- lucidExample :: IO JQuery -- renderLucid $ div_ $ do button ; table
lucidExample = do
  myClick <- select "<div>click here</div>"
  myShowHide <- select "<div>show/hide</div>"
  myCount <- select "<div>1</div>"
  myTable <- select $ renderLucid (table 1)
  counter <- newIORef (1::Int)
  let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', c'))
  let action _ = void $ do
        c <- getCount 
        setText (Text.pack . show $ c) myCount
        setHtml (renderLucid (table c)) myTable
  click action  def myClick
  toggleShow myShowHide myTable
  select "body" >>= appendJQuery myClick >>= appendJQuery myShowHide >>= appendJQuery myCount >>= appendJQuery myTable

-- button = button_ "Reload"

tshow = Text.pack . show

--table :: Int -> Html ()
--table n = t . div_ . ul_ . sequence_ . map (li_ . toHtml) . map tshow $ [1..n]

table :: Int -> Html ()
table n = t n (toHtml $ tshow n)


t :: Int -> Html () -> Html()
t n = table_ . mapM_ tr_ . replicate n . mapM_ td_ . replicate n

toggleShow a b = do
  showFlag <- newIORef (False :: Bool)
  let action _ = void $ do
        status <- atomicModifyIORef showFlag (\c -> let c' = not c in (c', c'))
        if status then JQuery.hide b 
          else JQuery.show b
  click action def a
