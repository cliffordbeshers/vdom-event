{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module LucidExample where

import Control.Applicative
import Data.List
import Data.Text as Text (Text, pack)
import Data.Text.Lazy (toStrict)
import JavaScript.JQuery
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
  myCount <- select "<div>1</div>"
  myTable <- select $ renderLucid (table 1)
  counter <- newIORef (1::Int)
  let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', c'))
  let action _ = void $ do
        c <- getCount 
        setText (Text.pack . show $ c) myCount
        setHtml (renderLucid (table c)) myTable
  click action  def myClick
  select "body" >>= appendJQuery myClick >>= appendJQuery myCount >>= appendJQuery myTable

-- button = button_ "Reload"

tshow = Text.pack . show

--table :: Int -> Html ()
--table n = t . div_ . ul_ . sequence_ . map (li_ . toHtml) . map tshow $ [1..n]

table :: Int -> Html ()
table n = t n (toHtml $ tshow n)


t :: Int -> Html () -> Html()
t n = table_ . mapM_ tr_ . replicate n . mapM_ td_ . replicate n

