{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module LucidExample where

import Control.Applicative
import Data.List
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack, intercalate, concat)
import Data.Text.Lazy (toStrict)
#if CLIENT
import JavaScript.JQuery as JQuery (JQuery, appendJQuery, click, select, setHtml, setText)
import qualified JavaScript.JQueryExtra as JQuery (hide, show)
import qualified JavaScript.JQueryUI as JQueryUI (sortable)
#else
import JavaScript.JQueryServer as JQuery
#endif
import Control.Monad
import Data.Default
import Data.IORef

import Lucid
import Data.Tree
import ZipTree

#if CLIENT
renderLucid :: Html () -> Text
renderLucid = toStrict . renderText

-- TODO state monad replacing url state
--   what encoding to use?

-- Technology demonstration menu

-- Term arg result => arg -> result
title :: Monad m => Text -> HtmlT m () -> HtmlT m ()
title s m =
  div_ $ do
    h2_ $ toHtml s
    m

submenu :: Monad m => HtmlT m () -> HtmlT m ()
submenu = div_

instance ToHtml () where
  toHtml () = toHtml ("" :: String)
  toHtmlRaw () = toHtml ("" :: String)


-- techDemos :: Monad m => HtmlT m ()
techDemos = do
  title "State Monad replacing URL" $ submenu $ do
    title "Back button functions with State Monad" $ return ()
    title "Shareable url" $ return ()
  title "All the bootstrap things" $ submenu $ do
    title "grid examples" $ return ()
    title "Labeling text edit widgets" $ return ()
  title "Wysiwyg" $ submenu $ do
    title "Stock example" $ return ()
    title "Minimal example" $ return ()
    title "Minimal with readonly sections" $ return ()
  title "Popups and dialogs and zoomins" $ submenu $ do
    title "popups" $ return ()
    title "dialogs" $ return ()
    title "zoomins" $ return ()
  title "Edit history" $ submenu $ do
    title "Expandable history list" $ return ()
    title "Showing what is locally saved versus globally" $ return ()
    title "Last communication with users, server" $ return ()
  title "Admin stuff" $ submenu $ do
    title "Sudo" $ return ()
    title "Last communication with users, server" $ return ()
  title "Complicated interactions" $ submenu $ do
    title "Permuation Lists" $ submenu $ do
      title "Basic widget" $ return ()
      title "Should the items be read only?" $ return ()
      title "How to delete?" $ return ()
  title "Server communications" $ submenu $ do
    title "Global, show server status" $ return ()
    title "Show anything unsaved" $ return ()
    title "Show conflicts, links to zoom in." $ return ()
    title "Show valid" $ return ()
  title "Lucid Example" $ lucidExample2

lucidExample2 :: Html ()
lucidExample2 = do
  div_ $ "Hello"


lucidExample :: IO JQuery -- renderLucid $ div_ $ do button ; table
lucidExample = do
  myClick <- select "<div>click here</div>"
  myShowHide <- select "<div>show/hide</div>"
  myCount <- select "<div>1</div>"
  myTable <- select $ renderLucid (table 1)
  techDemos' <- select $ renderLucid techDemos
  mySList' <- select $ renderLucid (slistB 5)
  mySList <- select "<div></div>" >>= appendJQuery mySList'
  counter <- newIORef (1::Int)
  let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', c'))
  let action _ = void $ do
        c <- getCount 
        setText (Text.pack . show $ c) myCount
        setHtml (renderLucid (table c)) myTable
  click action  def myClick
  toggleShow myShowHide myTable
  JQueryUI.sortable mySList'
  select "body" >>= appendJQuery techDemos' >>= appendJQuery mySList >>= appendJQuery myClick >>= appendJQuery myShowHide >>= appendJQuery myCount >>= appendJQuery myTable 

-- button = button_ "Reload"

tshow = Text.pack . show

-- xs = [(ul_,[]),(li_,["ui-state-default"]), (span_, ["ui-icon","ui-icon-arrowthick-2-n-s"])]

type TagList = Term arg result => [arg -> result]

tags :: TagList
tags = [div_, ul_, li_, span_]

classes :: [[Text]]
classes = [[], [],["ui-state-default"], ["ui-icon","ui-icon-arrowthick-2-n-s"]]

--slist' :: [Text] -> Html ()
--slist' xs = map foldlzipWith ($)

slist :: Int -> Html ()
slist n = ul_ . sequence_ . map (li_ . toHtml) . map tshow $ [1..n]

slistB :: Int -> Html ()
slistB n = (ul_ `with` [class_ $ classes !! 0]) . sequence_ . map ((li_ `with` [class_ (classes !! 1)]) ) . map ((>> dragArrow) . toHtml . ("Hoonan Item " <>) . tshow) $ [1..n]
  where classes = map (Text.intercalate " ") $ zipWith (\a b -> nub . sort $ a ++ b) listGroup uiState
        listGroup = [ ["list-group"], ["list-group-item"] ]
        uiState = [ [], ["ui-state-default", "btn", "btn-default"] ]
        dragArrow :: Html ()
        dragArrow = span_ [class_ $ Text.intercalate " " ["ui-icon","ui-icon-arrowthick-2-n-s"]] (toHtml "")

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
#endif
