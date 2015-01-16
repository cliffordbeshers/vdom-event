{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Clckwrks.AVR3.WishList (WishListURL(..), wishList) where

import Data.Data (Data)
import Data.Text (Text)
import qualified Data.Text as T (empty)
import Data.Tree (Forest, Tree(..))
import Data.Typeable (Typeable)
import Text.Blaze.Html5 (Markup, ToMarkup(..))
import qualified Text.Blaze.Html5 as H (li, ToMarkup(toMarkup), ul)
import Web.Routes.TH (derivePathInfo')

default (Text)

data WishListURL = WishListURL
                 deriving (Eq, Ord, Read, Show, Typeable, Data)

$(derivePathInfo' id ''WishListURL)

forestToMarkup :: Forest Text -> Markup
forestToMarkup [] = toMarkup T.empty
forestToMarkup ts = H.ul . (mapM_ treeToMarkup) $ ts

treeToMarkup :: Tree Text -> Markup
treeToMarkup (Node t cs) = do H.li (H.toMarkup t)
                              forestToMarkup cs

  -- make an outline mode.


-- wishList :: FilterMonad Response m => WishListURL -> ClckT WishListURL m Markups
wishList :: Monad m => WishListURL -> m Markup
wishList u =
  case u of
    WishListURL -> return $ forestToMarkup wishes

wishes :: Forest Text
wishes = [ g "Administration console."
           [ w "Build information."
           , w "Who is logged in."
           , w "Uptime"
           , w "MOTD support"
           ]
         , g "Haskell idioms"
           [ w "mvar/controlled show/read for dynamic load"
           , w "monadic imports"
           , w "yield"
           , w "ZipTrees for layout"
           ]
         , g "Offline Editing"
           [ w "Events"
           , w "Undo"
           , w "timestamps"
           , w "local storage format"
           , w "graphical feedback for online/offline"
           , w "mergable documents"
           , w "clock skew"
           ]
         , g "HTML/CSS"
           [ w "Catalogue inline versus block."
           , w "blaze -> sunroof"
           , w "yield"
           ]
         , g "Drag/Drop"
           [ w "haskell type as attribute"
           , w "baby example"
           ]
         , g "Miscellaneous"
           [ w "Misc"
           ]
         ]
  where w t = Node t []
        g = Node
