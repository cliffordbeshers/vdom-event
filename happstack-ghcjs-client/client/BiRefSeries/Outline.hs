{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Outline where

import Data.Text as Text (Text)
import Control.Monad.Reader
import Control.Monad.Trans.Free
import Data.Tree
import Lucid

default (Text)


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


type Outline = Monad m => HtmlT m ()

type Outline' m a =  ReaderT r FreeT m Tree a

type Tree' = Free Tree

y :: MonadFree Tree m => a -> m a
y t = liftF $ Node t []

y' :: MonadFree Tree m => a -> m a
y' = return

wrap' :: MonadFree Tree m => Tree (m a) -> m a
wrap' = wrap

title' = Node
submenu' = Node

zzz :: MonadFree Tree m => m Text
zzz = wrap' $ Node (return "a") []

-- yy :: MonadFree Tree m => m Text
ttt :: Tree Text
ttt = Node "a" [Node "b" [], Node "c" []]


--yy :: MonadFree Tree m => m Text
--yy = liftF $ Node "hello" [liftF (Node (return "c1") []), liftF (Node (return "c2") [])]

--z :: Monad m => Outline' m Text
--z = return "Hello"
  
-- y :: Outline'
-- y :: Tree (Outline')
-- y = Node _foo -- Node z

-- w :: Outline'
-- w = return (Node "hello" []) >>= (return . Node "goo" . (:[]))
  



techDemos :: Outline
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
