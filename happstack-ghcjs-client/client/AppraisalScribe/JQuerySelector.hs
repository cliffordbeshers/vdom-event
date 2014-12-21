{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module AppraisalScribe.JQuerySelector (JSelector(..)
                      , HtmlElement(..)
                      , cssUI
                      , render
                      , smush
                      ) where

import Data.Data (Data)
import Data.Monoid ((<>), Monoid(..))
import Data.Text as Text (Text, cons, intercalate, pack, unpack)
import Data.Typeable (Typeable)
import Data.List (intercalate)

import Language.Javascript.JMacro

import AppraisalScribe.CSS
import AppraisalScribe.Reindeer

default (Text)
-- http://api.jquery.com/category/selectors/

data JSelector = ById JSelector
               | ByClass JSelector JSelector
               | ByElement HtmlElement
               | Ident Text
               | Child JSelector JSelector
               | Descendant JSelector JSelector
               | Disabled JSelector
               | Enabled JSelector
               | None
                 deriving (Eq, Ord, Read, Show, Typeable, Data)

data HtmlElement = SPAN | UL | LI | DIV | BUTTON deriving (Eq, Ord, Read, Show, Typeable, Data)

cssUI :: JSelector -> [(Text,Text)] -> Text
cssUI sel pats = render sel <> " " <> formatCSS pats


render :: JSelector -> Text
render (ById s) = '#' `cons` (render s)
render (ByClass s c) = mconcat [render s, "." , render c]
render (Ident s) = s
render (Child p c) = smush [render p, ">", render c]
render (Descendant p c) = smush [render p, render c]
render (Disabled s) = smush [":disabled", render s]
render (Enabled s) = smush [":enabled", render s]
render (ByElement he) = reindeer he
render (None) = ""

smush :: [Text] -> Text
smush = Text.intercalate " "

instance Reindeer JSelector String where
    reindeer = unpack . render

instance Reindeer JSelector Text where
    reindeer = render

instance Reindeer HtmlElement String where
    reindeer = show

instance Reindeer HtmlElement Text where
    reindeer = pack . show

instance ToJExpr Text where
    toJExpr = toJExpr . unpack

instance ToJExpr JSelector where
    toJExpr = toJExpr . render

instance ToJExpr [JSelector] where
    toJExpr = toJExpr . Text.intercalate ", " . map render

#if UNUSED
testme :: String
testme = reindeer (Descendant (ById (Ident "foo")) (ByClass None (Ident "bar")))

testJExpr :: JStat
testJExpr = [jmacro|$(`(ById (Ident "foo"))`).foo();|]
#endif
