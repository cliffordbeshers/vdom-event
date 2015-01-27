{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module WebModule.SortableModule (sortableWebModule, SortableBindings(..)
                                , SortableOperation(..), Error(..), update) where

import Data.Aeson as Aeson (decode)
-- import Common
import WebModule.ModuleScopeURL
import WebModule.WebModule
import WebModule.WebModuleM
import WebModule.AJAXModule
import Control.Monad.Trans
import System.FilePath ((</>))
import Data.Aeson
import GHC.Generics
import Data.Default
import Data.List (sortBy)
import Data.Typeable (Proxy(..))
import WebModule.GName
#ifdef CLIENT
import JavaScript.JQuery as J
import WebModule.JQueryWebModule as JQuery
import WebModule.JQueryUIWebModule as JQuery
#else
import WebModule.GHCJSStub.JQuery as J
#endif
#ifdef SERVER
import Happstack.Server
#endif

-- jquery sortable.
-- construct with a list of keys
-- handle a permuted list of those keys
-- handle a (Move i j) event, defined as: remove position i, 0 \le i \lt length, reinsert at position j, 0 \le j \lt length.

import Text.Blaze.Html5 as H (Markup, toMarkup, ul, li)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as LT (Text, unpack, pack, toStrict)
import Data.Text as T (Text, pack)


data SortableBindings = SortableBindings

sortableWebModule :: Monad m => WebSiteM m SortableBindings
#if CLIENT
sortableWebModule = return SortableBindings
#else 
sortableWebModule = wimport ws SortableBindings
  where ws = mzeroWebSite { serverpart = sortableHandler
                          , headers = [] 
                          , bodies = []
                          , baseURL = []
                          }
#endif

baseurl:: ModuleScopeURL
#if CLIENT
baseurl = moduleScopeURL "WebModule.SortableModule" ""
#else
baseurl = $(moduleScopeURL "")
#endif

basepath :: FilePath
basepath = moduleScopeURLtoFilePath baseurl

messageKey :: String
messageKey = gname (Proxy :: Proxy SortableOperation)


#if SERVER
sortableHandler :: ServerPartT IO Response
sortableHandler = dirs basepath $ h
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
#endif


data SortableOperation = Move Int Int | Permutation [Int] deriving (Eq, Show, Generic)

instance FromJSON SortableOperation
instance ToJSON SortableOperation

data Error = ErrorMoveOutOfRange | ErrorPermutation deriving (Eq, Show, Generic)


blaze :: Markup -> IO JQuery
blaze = select . LT.toStrict . renderHtml 

sortableMarkup :: IO JQuery
sortableMarkup = blaze $ H.ul $ sequence_ $ map (H.li . H.toMarkup)  $ map (\n -> "Item " ++ show n) [1..4]

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
