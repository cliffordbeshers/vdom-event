{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS -fwarn-incomplete-patterns #-}
module LucidExample where

import Control.Applicative
import Data.List
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack, intercalate, concat)
import Data.Text.Lazy (toStrict)
#if CLIENT
import JavaScript.JQuery as JQuery (JQuery, appendJQuery, click, select, setHtml, setText, getHtml, setAttr, getAttr, removeAttr)
import qualified JavaScript.JQueryExtra as JQuery (hide, show)
import qualified JavaScript.JQueryUI as JQueryUI (sortable)
#else
import JavaScript.JQueryServer as JQuery
#endif
import Control.Monad
import Control.Monad.Trans
import Data.Default
import Data.IORef

import Lucid
import Data.Tree
import ZipTree
import Outline
import Radio
-- import AppraisalScribe.UIClient

default (Text)

#if CLIENT
renderLucid :: Html () -> Text
renderLucid = toStrict . renderText

-- TODO state monad replacing url state
--   what encoding to use?

-- Technology demonstration menu

-- Term arg result => arg -> result

submenu :: Monad m => HtmlT m () -> HtmlT m ()
submenu = div_

instance ToHtml () where
  toHtml () = toHtml ("" :: String)
  toHtmlRaw () = toHtml ("" :: String)

-- techDemos :: Monad m => HtmlT m ()
techDemos :: Forest Text
techDemos = let title = Node in [
  title "State Monad replacing URL"
    [ title "Back button functions with State Monad" [] 
    , title "Shareable url" []
    ]
  ,title "All the bootstrap things"
    [ title "grid examples" []
    , title "Labeling text edit widgets" []
    ]
  ,title "Wysiwyg"
    [ title "Stock example" []
    , title "Minimal example" []
    , title "Minimal with readonly sections" []
    ]
  ,title "Popups and dialogs and zoomins"
    [ title "popups" []
    , title "dialogs" []
    , title "zoomins" []
    ]
  ,title "Edit history"
    [ title "Expandable history list" []
    , title "Showing what is locally saved versus globally" []
    , title "Last communication with users, server" []
    ]
  ,title "Admin stuff"
    [ title "Sudo" []
    , title "Last communication with users, server" []
    ]
  ,title "Complicated interactions"
    [ title "Permuation Lists"
      [ title "Basic widget" []
      , title "Should the items be read only?" []
      , title "How to delete?" []
      ]
    ]
  ,title "Server communications"
    [ title "Global, show server status" []
    , title "Show anything unsaved" []
    , title "Show conflicts, links to zoom in." []
    , title "Show valid" []
    ]
  ,title "Good old bugs"
    [ title "You must leave space at the end of the document/page or interactions are weird" []
    , title "How do you get anchors to show up somewhere other than the top of the page?" []
    , title "Show conflicts, links to zoom in." []
    , title "Show validated fields." []
    ]
  ]

buttonMarkup :: Monad m => Ident -> HtmlT m ()
buttonMarkup b = div_ [id_ b] $ "Hide/Show Button"

targetMarkup :: Monad m => Ident -> HtmlT m ()
targetMarkup t = div_ [id_ t] $ "Hide/Show Target"

hideshow2 :: Outline ()
hideshow2 = do
  b <- new
  buttonMarkup b
  t <- new
  targetMarkup t
  bind $ ToggleShow b t

storeGC :: MonadIO m => Ident -> m () -> m ()
storeGC ident gc = return ()
-- writeIOref.

evalOp :: MonadIO m => Op -> m ()
evalOp op = liftIO $
  case op of
    ToggleShow a b -> do
      a' <- select (byId a)
      b' <- select (byId b)
      gc <- toggleShow a' b'
      storeGC a gc
      storeGC b gc
      return ()
    SetAttr a l v -> do
      a' <- select (byId a)
      _j <- setAttr l v a'
      return ()
    ClearAttr a l -> do
      a' <- select (byId a)
      _j <- removeAttr l a'
      return ()
    Click a ops -> do
      a' <- select (byId a) -- ???
      mapM_ evalOp ops
      return ()

load :: Outline a -> IO JQuery
load o = do
  let (t, ops) = renderOutline o
  n <- select t -- use Jquery to turn the html into a node.
  -- could/should these next two lines be inverted?
  j <- select "body" >>= appendJQuery n -- attach to the dom
  mapM_ evalOp ops -- run initializiations.
  return j

loadT :: (Functor m, MonadIO m) => OutlineT m a -> m ()
loadT o = do
  (html, ops) <- renderOutlineT o
  liftIO $ do
    n <- select html -- use Jquery to turn the html into a node.
    -- could/should these next two lines be inverted?
    j <- select "body" >>= appendJQuery n -- attach to the dom
    mapM_ evalOp ops -- run initializiations.

loadText :: Text -> IO JQuery
loadText t = do
  t' <- select t
  b <- select "body"
  appendJQuery t' b
  return t'

-- counterOutline :: Outline ()
  -- myClick <- loadText "<div>click here</div>"
  -- myCount <- loadText "<div>1</div>"
  -- myTable <- loadText $ renderLucid (table 1)
  -- counter <- newIORef (1::Int)
  -- let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', c'))
  -- let action _ = void $ do
  --       c <- getCount 
  --       setText (Text.pack . show $ c) myCount
  --       setHtml (renderLucid (table c)) myTable
  -- click action  def myClick
  -- return myTable

lucidExample :: (Functor m, Monad m, MonadIO m) => m () -- renderLucid $ div_ $ do button ; table
lucidExample = loadT radioExample
--  load radioExample
  -- myClick <- loadText "<div>click here</div>"
  -- myCount <- loadText "<div>1</div>"
  -- myTable <- loadText $ renderLucid (table 1)
  -- counter <- newIORef (1::Int)
  -- let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', c'))
  -- let action _ = void $ do
  --       c <- getCount 
  --       setText (Text.pack . show $ c) myCount
  --       setHtml (renderLucid (table c)) myTable
  -- click action  def myClick
  -- return myTable
  --  load hideshow2
  
--  myShowHide <- loadText "<div>show/hide</div>"
--  load (outline techDemos)
--   mySList' <- select $ renderLucid (slistB 5)
--   mySList <- select "<div></div>" >>= appendJQuery mySList'
--   toggleShow myShowHide myTable
--   JQueryUI.sortable mySList'
-- -- appendJQuery techDemos' >>= 
--   select "body" >>= appendJQuery mySList

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

toggleShow :: JQuery -> JQuery -> IO (IO ())
toggleShow a b = do
  showFlag <- newIORef (False :: Bool)
  let action _ = void $ do
        status <- atomicModifyIORef showFlag (\c -> let c' = not c in (c', c'))
        if status then JQuery.hide b 
          else JQuery.show b
  -- emphasize that the return value is the gc action
  gc <- click action def a
  return gc

-- radioButton ::  -> IO (IO ())
-- radioButton xs = do
--   showFlag <- newIORef (False :: Bool)
--   let action _ = void $ do
--         status <- atomicModifyIORef showFlag (\c -> let c' = not c in (c', c'))
--         if status then JQuery.hide b 
--           else JQuery.show b
--   -- emphasize that the return value is the gc action
--   gc <- click action def a
--   return gc

#endif
