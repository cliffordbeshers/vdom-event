{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module SortableModule (sortableWebModule, SortableBindings(..), SortableOperation(..)) where

-- jquery sortable.

-- construct with a list of keys
-- handle a permuted list of those keys
-- handle a (Move i j) event, defined as: remove position i, 0 \le i \lt length, reinsert at position j, 0 \le j \lt length.


import Data.Aeson
import GHC.Generics
import Data.Default
import Data.List (sortBy)
#ifdef CLIENT
import JavaScript.JQuery as J
#else
import GHCJSStub.JQuery as J
import Happstack.Server
import System.FilePath ((</>))
#endif

import WebModule.JQueryWebModule as JQuery
import WebModule.JQueryUIWebModule as JQuery

import Text.Blaze.Html5 as H (Markup, toMarkup, ul, li)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as LT (Text, unpack, pack, toStrict)
import Data.Text as T (Text, pack)
import Data.Text.Encoding as TE (decodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Aeson as Aeson (decode, encode)

import Common
import WebModule.WebModule
import WebModule.WebModuleM
import Control.Monad.Trans

data SortableBindings = SortableBindings { move :: Int -> Int -> IO AjaxResult
                                         , markup :: [Markup] -> Markup

                                         }

data SortableOperation = Move Int Int | Permutation [Int] deriving (Eq, Show, Generic)

instance FromJSON SortableOperation
instance ToJSON SortableOperation


data Error = ErrorMoveOutOfRange | ErrorPermutation deriving (Eq, Show, Generic)

ajaxJSON :: ToJSON a => T.Text -> a -> IO AjaxResult
ajaxJSON url a = ajax url [(T.pack messageKey, tj a)] def
  where tj :: ToJSON a => a -> T.Text
        tj = TE.decodeUtf8 . toStrict1 . Aeson.encode
        toStrict1 :: BL.ByteString -> B.ByteString
        toStrict1 = B.concat . BL.toChunks


sortableBindings = SortableBindings { move = mf , markup = sortableMarkup }
  where mf i j = ajaxJSON ajaxURLT $ Move i j

        sortableMarkup :: [Markup] -> Markup
        sortableMarkup = H.ul . sequence_ . map H.li


-- This needs to wimport jQuery and jQueryUI

sortableWebModule :: Monad m => WebSiteM m SortableBindings
#if CLIENT
sortableWebModule = return sortableBindings
#else 
sortableWebModule = do
  _ <- jQueryWebModule -- should this be require?
  _ <- jQueryUIWebModule
  wimport ws sortableBindings
  where ws = mzeroWebSite { serverpart = sortableHandler
                          , headers = [] 
                          , bodies = []
                          , baseURL = []
                          }
#endif

#if SERVER
sortableHandler :: ServerPartT IO Response
sortableHandler = dirs ajaxURL $ h
  where h = do
          rq <- askRq
          liftIO $ print rq
          let rqpath = foldr (</>) "" $ rqPaths rq
          liftIO $ print rqpath
          decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
          msgValue <- lookBS $ messageKey
          let msg :: Maybe SortableOperation = decode msgValue
          liftIO $ do
            putStrLn "\n\n\nThe message is:"
            print msg
            putStrLn "\n\n\n"
          ok $ toResponse "sortableHandler response"

ajaxHandler :: ServerPartT IO Response
ajaxHandler = dirs ajaxURL $ h
  where h = do
          rq <- askRq 
          liftIO $ print rq
          let rqpath = foldr (</>) "" $ rqPaths rq
          liftIO $ print rqpath
          decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
          msgValue <- lookBS $ messageKey
          let msg :: Maybe MarshalMe = decode msgValue
          liftIO $ do
            putStrLn "\n\n\nThe message is:"
            print msg
            putStrLn "\n\n\n"
          ok $ toResponse "life model decoy"
#endif

blaze :: Markup -> IO JQuery
blaze = select . LT.toStrict . renderHtml 

-- initialize :: JQuery -> IO JQuery
initialize = J.on (\e -> putStrLn "Sortable.hs: update called") sortUpdate def

sortUpdate :: T.Text
sortUpdate = T.pack "sortupdate"


update :: SortableOperation -> [key] -> Either Error [key]
update op ks =
  case op of
    Move i j -> 
      let l = length ks in
      if i < 0 || i >= l || j < 0 || j >= l then
        Left ErrorMoveOutOfRange
      else let x = ks !! i in
      Right (insertAt x j (deleteAt i ks))
    Permutation ps -> 
      if (length ps /= length ks) then
        Left ErrorPermutation
      else
        Right (permute ps ks)
                    
permute :: [Int] -> [a] -> [a]
permute ps ks = snd . unzip . (sortBy (\(a,_) (b,_) -> compare a b)) $ zip ps ks

insertAt :: a -> Int -> [a] -> [a]
insertAt x j ys =
  let (l,r) = splitAt j ys
  in l ++ [x] ++ r

deleteAt :: Int -> [a] -> [a]
deleteAt i ys =
  let (l,r) = splitAt i ys
  in l ++ tail r
