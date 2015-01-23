{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Bootstrap.Tabs where

-- http://getbootstrap.com/javascript/#tabs

import Control.Monad (foldM)
import Data.Text as Text (Text)
import Lucid
import Bootstrap.Operations
import Bootstrap.Utils (aria_controls_, classes_, role_)
import Data.Supply (modifySupply, newNumSupply, split, split2, split3, Supply, supplyValue)

-- main = do
--   s0 <- newNumSupply
--   let (s1,s2,s3) = split3 s0
--   let ss = split s2
--   print $ supplyValue s0
--   print $ supplyValue s1
--   print $ supplyValue s2
--   print $ take 5 $ map supplyValue ss
--   print $ take 5 $ map supplyValue (split s1)
--   let s4 = modifySupply s3 (("prefix" ++) . show . supplyValue)
--   let (s5,s6) = split2 s4
--   print $ supplyValue s4
--   print $ take 5 $ map supplyValue (split s5)
--   print $ take 5 $ map supplyValue (split s6)

-- prefixSupply :: Show a => String -> Supply a -> Supply String
-- prefixSupply prf sup = modifySupply sup ((prf ++) . show . supplyValue)

-- newPrefixSupply :: String -> IO (Supply String)
-- newPrefixSupply prefix = do
--   s0 <- newNumSupply
--   return $ prefixSupply prefix s0


default (Text.Text)

bigTable :: [[Int]] -> Html ()
bigTable t = table_ (mapM_ row t)
  where row :: [Int] -> Html ()
        row r = tr_ (mapM_ (td_ . toHtml . show) r)

exampleTabs :: MonadIO m => HtmlT m ()
exampleTabs = do
   ids <- fmap split $ newPrefixSupply
   

tabdata :: [(Text, Text)]
tabdata = [("Title One","Content One"),("Title Two","Content Two"),("Title Three","Content Three")]

tabdata' :: Supply Int -> [(a,b)] -> [((Int,a),(Int, b))]
tabdata' s ps = let (as,bs) = unzip ps in zip (zip ss0 as) (zip ss1 bs)
  where (s0, s1) = split2 s
        ss0 = map supplyValue (split s0)
        ss1 = map supplyValue (split s1)

titles :: [Html ()]
titles = map (toHtml . fst) tabdata

panes :: [Html ()]
panes = map (toHtml . snd) tabdata


mkTabs :: [(Html (), Html ())] -> Html ()
mkTabs ts = dv . ul . mapM_ (li . a) $ map fst ts
  where dv = div_ [role_ "tabpanel"]
        ul = ul_ [role_ "tablist", classes_ ["nav","nav-tabs"]]
        li = li_ [role_ "presentation", classes_ ["active"]]
        a = a_ [href_ "#", aria_controls_ "settings", role_ "tab",  data_ "toggle" "tab"]
--        each :: (Term a b, Term b c) => a -> c
--        each = (li_ . a_ . toHtml)

-- data H3 = Tag3 (Term arg result => arg -> result) [Attribute] [H3] | Leaf Text

-- -- tabsH3 :: [(Term arg result => arg -> result, [Attribute])]
-- tabsH3 :: [[H3] -> H3]
-- tabsH3 = [ Tag3 div_ [role_ "tabpanel"]
--          , Tag3 ul_  [role_ "tablist", classes_ ["nav","nav-tabs"]]
--          , Tag3 li_  [role_ "presentation", classes_ ["active"]]
--          , Tag3 a_   [role_ "presentation", classes_ ["active"]]
--          ]



-- tabs :: Term arg result => [(arg -> result, [Attribute])]
-- tabs = [ (div_, [role_ "tabpanel"])
--        , (ul_, [role_ "tablist", classes_ ["nav","nav-tabs"]])
--        , (li_, [role_ "presentation", classes_ ["active"]])
--        , (a_, [role_ "presentation", classes_ ["active"]]) ]



-- panes :: Term arg result => [(arg -> result, [Attribute])]
-- panes = [ (div_, [classes_ ["tab-content"]])
--         , (div_, [role_ "tabpanel",  classes_ ["tab-pane", "active"]])
--         ]


-- tabs' :: (Term arg result, With (arg -> result)) => [arg -> result]
--tabs' :: (Term arg result, With (arg' -> result)) => [(arg -> result, [Attribute])] -> [arg' -> result]
--tabs' = map (curry with)

--z :: (Term a r) => [(a->r,[Attribute])] -> b -> c
--z [a,b] h = a `f` b `f` h

-- yy :: Html ()
--yy = z panes h

-- f :: a -> b
--f = undefined

h :: Html ()
h = div_ "html"


--z :: Html () -> Html ()
--z = (fst . head $ panes) `with` (snd . head $ panes)