{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
module Bootstrap.Operations where

import Data.List as List (nub,zipWith)
import Data.Text as Text (Text, intercalate, pack, unpack)
import Lucid

default (Text)

data H = Tag Text [Attr] [H]
data Attr = Attr Text Text

data H2 = Tag2 (Term arg result => arg -> result) [Attr2] [H2] | Leaf Text
data Attr2 = Attr2 (Text -> Attribute) Text

type TagList = Term arg result => [arg -> result]

instance Show H2 where
  show _ = "H2 show instance not implemented"


tags :: TagList
tags = [div_, ul_, li_, span_]

tags' :: [ [Attr] -> [H] -> H ]
tags' = [Tag "div", Tag "ul", Tag "li", Tag "span"]

tags2 :: [ [Attr2] -> [H2] -> H2 ]
tags2 = [Tag2 div_, Tag2 ul_, Tag2 li_, Tag2 span_]

styles :: [[Attribute]]
styles = map (listify . class_s) [[], [],["ui-state-default"], ["ui-icon","ui-icon-arrowthick-2-n-s"]]

styles' :: [[Attr]]
styles' = map (listify . class_s') [[], [],["ui-state-default"], ["ui-icon","ui-icon-arrowthick-2-n-s"]]

styles2 :: [[Attr2]]
styles2 = map (listify . class_s2) [[], [],["ui-state-default"], ["ui-icon","ui-icon-arrowthick-2-n-s"]]

listify :: a -> [a]
listify = (:[])

class_s :: [Text] -> Attribute
class_s = class_ . foldClasses
  where foldClasses :: [Text] -> Text
        foldClasses = Text.intercalate " " . nub

class_s' :: [Text] -> Attr
class_s' = Attr "class" . foldClasses
  where foldClasses :: [Text] -> Text
        foldClasses = Text.intercalate " " . nub

class_s2 :: [Text] -> Attr2
class_s2 = Attr2 class_ . foldClasses
  where foldClasses :: [Text] -> Text
        foldClasses = Text.intercalate " " . nub


textshow :: Show a => a -> Text
textshow = Text.pack . show

wrap :: With (s -> t) => (s -> t) -> [Attribute] -> s -> t
wrap t a m = t `with` a $ do m

--fs :: Term arg result => [arg -> result]
--fs :: (Term arg result, Term [Attribute] result) => [arg -> result] -> [[Attribute]] -> [result]

zipAp :: [ a -> b ] -> [a] -> [b]
zipAp [] _ = []
zipAp _ [] = []
zipAp (t:ts) (s:ss) = (t s) : (zipAp ts ss)

widget :: (Term arg result, With (arg -> result)) => [arg -> result]
widget = zipWith with tags styles

-- widget' :: (Term arg result, With (arg -> result)) => [arg -> result]
widget2 :: [[H2] -> H2]
widget2 = zipWith ($) tags2 styles2

-- widget' :: (Term arg result, With (arg -> result)) => [arg -> result]
widget' :: [[H] -> H]
widget' = zipWith ($) tags' styles'


foldH2 :: [[H2] -> H2] -> [H2] -> H2
foldH2 [] = error "foldH2 cannot handle an empty list, because there is no tag specified"
foldH2 (h:[]) = h
foldH2 (h:hs) = h . listify . foldH2 hs 

foldH2' :: [[H2] -> H2] -> [H2] -> H2
foldH2' = foldr1 (\f g -> f . listify . g)


foldHtml :: Monad m => TagList -> [HtmlT m a] -> HtmlT m a
foldHtml = undefined

zzz :: Monad m => [HtmlT m a] -> HtmlT m a
zzz = foldHtml tags

test :: Bool
test = show left == show right
  where left :: Html ()
        left = ul_ $ sequence_ $ map (li_ . toHtml) xs
        right = (foldH2 widget2) (map Leaf xs)
        xs = map textshow [0..4 :: Int]

-- fs :: Monad m => [HtmlT m a]
-- fs = zipWith ($) tags attrs 
--   where attrs = map (:[]) (foldClasses classes)

--slist' :: [Text] -> Html ()
--slist' xs = map foldlzipWith ($)
