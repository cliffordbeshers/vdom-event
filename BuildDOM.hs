{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
module BuildDOM where

import Control.Monad (void, when)
import Data.HashMap.Strict as HashMap (toList)
import Data.Maybe (fromJust, isJust)
import Data.Text as Text (Text, intercalate)
import Data.Tree
import GHCJS.Foreign
import GHCJS.Types

import Alderon.Html
import Alderon.Html.Internal
import MicroDOM

default (Text)

buildDOM :: Html -> IO [JSRef DOMElement]
buildDOM h = do
  let fs = runHtml h
  buildForest fs

buildForest :: Forest Node -> IO [JSRef DOMElement]
buildForest = mapM buildNode

buildNode :: Tree Node -> IO (JSRef DOMElement)
buildNode (Node (Element tag attrs) fs) = do
  n <- createElement tag
  setAttributes n attrs
  return n
buildNode (Node (TextNode t) fs) = do
  createTextNode t

setAttributes :: JSRef DOMElement -> Attributes -> IO ()
setAttributes n (Attributes{..}) = do
  -- This does not clear the id if already set and elementId is Nothing.
  when (isJust elementId) (void $ setAttribute n "id" (fromJust elementId))
  -- Ignore the key element, I think it is for sodium tracking.
  setAttribute n "class" (Text.intercalate " " elementClass)
  mapM_ (uncurry (setAttribute n)) (HashMap.toList otherAttributes)
