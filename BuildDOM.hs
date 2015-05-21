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

buildDOM :: Html -> IO [DOMElement]
buildDOM h = do
  let fs = runHtml h
  buildForest fs

buildForest :: Forest Node -> IO [DOMElement]
buildForest = mapM buildNode

buildNode :: Tree Node -> IO DOMElement
buildNode (Node (Element tag attrs) fs) = do
  n <- createElement tag
  setAttributes n attrs
  setHandlers n attrs
  buildForest fs >>= appendChildren n
  return n
buildNode (Node (TextNode t) fs) = do
  createTextNode t

setHandlers :: DOMElement -> Attributes -> IO (IO ())
setHandlers e attrs = do
  let ehs = HashMap.toList $ handlers attrs
  gcs <- mapM f ehs
  return $ sequence_ gcs
    where f (et,eh) = eventTargetAddEventListener e (eventName et) False (\elm ev -> eh ev)

setAttributes :: DOMElement -> Attributes -> IO ()
setAttributes n (Attributes{..}) = do
  -- This does not clear the id if already set and elementId is Nothing.
  when (isJust elementId) (void $ setAttribute n "id" (fromJust elementId))
  -- Ignore the key element, I think it is for sodium tracking.
  setAttribute n "class" (Text.intercalate " " elementClass)
  mapM_ (uncurry (setAttribute n)) (HashMap.toList otherAttributes)

appendChildren :: DOMElement -> [DOMElement] -> IO ()
appendChildren node = mapM_ (appendChild node)

detachChildren :: DOMElement -> IO [DOMElement]
detachChildren node = go node
  where go n = do
          mc <- getFirstChild n
          case mc of
            Nothing -> return []
            Just c -> do
              removeChild n c
              cs <- go n
              return (c:cs)
          
          

