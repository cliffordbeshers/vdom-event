{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module AppraisalScribe.UI ( UI(..)
          , UIAttribute(..)
          , Align(..)
          , Side(..)
          , balance
          , clean
          , ems
          , label
          , labels
          , nullUI
          , renderHtml5
          , printHtml5
          , selector
          , scrollbars
          , tabSelect
          , tempcss
          , toId
          , treedepth
          , truncateW
          , uiheight
          , uiwidth
          , ullist
          , vlabel
          ) where

import Control.Arrow (second)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Char
import Data.Generics.Uniplate.Operations -- (transformBi)
import Data.Generics.Uniplate.Data ()-- (universe, descend, transform, universeBi)
import Data.List (intersperse, partition)
import Data.Monoid
import Data.Text as Text (Text, concat, cons, pack, unwords, words,unpack, length)
import Lucid
import Lucid.Base
import Language.Javascript.JMacro

import AppraisalScribe.ActionMenu (actionMenuMarkup)
import AppraisalScribe.CSS
import AppraisalScribe.JQuerySelector hiding (UL, LI)
import AppraisalScribe.JMacro
import AppraisalScribe.MyURL
import AppraisalScribe.Reindeer
import AppraisalScribe.AjaxFileUpload (ajaxFileUploadInit)
-- import qualified Html5.Attr as Html5A

default (Text)

data Align = Start | End | Justify | Center deriving (Read, Show, Eq, Ord, Typeable, Data)
data Side = TopSide | LeftSide deriving (Read, Show, Eq, Ord, Typeable, Data)

--          Sep [UI] | HSep [UI] | VSep [UI] |
data UI url = -- Escape Html5.Html5|
              FormMP url [UI url] |
              Upload Text JStat Int |
              AjaxUpload url Bool Text Text Text Text JExpr |
              -- AjaxUpload url multiple divID fileTag namespace buttonText |
              Submit Text |
              Label Text |
              Rabel Text |
              Div (UI url) |
              Span (UI url) |
              Union [UI url] |
              LabelRef (UI url) (UI url) |
              Txt [(Text,Text)] Text |
              Txt1 Text |
              NBTxt Text |
              Att [UIAttribute] (UI url) |
              AttData Text Text (UI url) |
              HR |
              BR |
              UL [UI url] |
              LI (UI url) |
              Title Text |
              P Text |
              TTextForm url JSelector (Maybe Text) |
              TTextFormW url JSelector Text (Maybe Text) |
              TTTextFormW  url JSelector JExpr Text (Maybe Text) |
              TTextAreaForm url JSelector (Int,Int) (Maybe Text) |
              FontSize Float (UI url) |
              Form [UI url] |
              FieldSet [UI url] |
              Legend (UI url) |
              GenericButton JStat (UI url) |
              Button url JStat (UI url) |
              ButtonRaw JStat (UI url) |
              DropDown  url JSelector [(Text, UI url)] Text |
              ActionMenu Text [(Text, JStat)] |
              Video url |
              Image url (Int,Int) |
              ImageUnbnd url |
              Thumb url (Int,Int) Text |
              ThumbCSS url Text |
              Cropper url (Int,Int) |
              Box [(Text,Text)] [UI url] | HBox [(Text,Text)] [UI url] | VBox [(Text,Text)] [UI url] |
              HHBox Align [UI url] | VVBox Align [UI url] |
              ZBox [UI url] |
              IFrame url |
--             PagePage url [UI url] (UI url) |
-- Partial url [UI url] (UI url) |  -- the problem with the Partial is imports.  It could reference things not
              DynTabSet Side [(UI url, url)] |
              TabSet Side Text [(UI url, UI url)] |
              TabSelect Side Text [UI url] (UI url) |
              CheckBox Bool | CheckBoxM (Maybe Bool) |
              RadioButton Text Text | -- No recursive UI, because it is too complicated.  This maybe the wave of the future.
              Glue Float Text |
              Spacer Text |
              MinMax Text Text |
              Editor |
              Link url (UI url) |
              LinkTab url (UI url) |
              Anchor url (UI url) |
              ID Text (UI url) |
              Ref Text (UI url) |
              Call url |
              ToDo (UI url) |
              Background Text (UI url) |
--              JSInline JStat |
--              JSFile url |
--              CSSFile url |
--              CSSInline String |
              CSSAttr [(Text,Text)] (UI url) |
              CSSAttrI [(Text,Text)] (UI url) |
              SlideOut url (UI url) |
              Clients [(Text,Text)] [UI url] [[UI url]] |
              TableOnly |
              Table [(Text,Text)] [UI url] [[UI url]] deriving (Show, Eq, Ord, Typeable, Data)

data UIAttribute = Class Text
                 | Style CSS
                 | ContentEditable
                 | Seamless
                 deriving (Read, Show, Eq, Ord, Typeable, Data)

nullUI :: UI a
nullUI = BR -- Totally lame, but it will help eradicate undefined's

consolidate :: [ UIAttribute ] -> [ UIAttribute ]
consolidate xs = [Class (collect cs')] ++ ncs
  where
    (cs, ncs) = partition isClass xs
    cs' = [ c | Class c <- cs ]
    collect = Text.unwords . Prelude.concat . map Text.words
    isClass (Class _) = True
    isClass _ = False

uiattr :: UIAttribute -> Attribute
uiattr (Class s) = class_ s
uiattr (Style ss) = style_ $ unCSS ss
uiattr (ContentEditable) = contenteditable_ "true"
uiattr (Seamless) = makeAttribute "frameBorder" "0"

scrollbars :: UI a -> UI a
scrollbars = CSSAttr [("overflow-y","scroll")]

align :: Show a => Text -> a -> Text
align t = (t <>) . Text.pack . (" " ++) . map toLower . show

label :: Text -> UI a -> UI a
label s ui = HHBox Justify [ Label s, Glue 10.0 "em", ui]

labels :: Text -> [UI a] -> UI a
labels s uis = HHBox Justify $ [ Label s, Glue 10.0 "em" ] ++ uis

vlabel :: Text -> UI a -> UI a
vlabel s ui = VVBox Start [ Label s, ui]

nbsp :: Text
nbsp = "&nbsp;"

lookupID :: ToURL a => Text -> UI a -> UI a
lookupID _s h = h

{-
insertID :: ToURL a => Text -> UI a -> UI a
insertID _s h = h
-}

clean :: (ToURL url, Show (URLT url)) => url -> Text
clean = pack . filter isAlphaNum . show . toURL

toId :: (ToURL url, Show (URLT url)) => url -> Text
toId = Text.pack . ("id_" ++) . filter isValidIdChar . map replaceEvil . show . toURL
       where replaceEvil c | c == '/' = '_'
                           | c == '%' = '_'
                           | otherwise = c
             isValidIdChar c = any id $ map ($ c) [isAlphaNum , (== '-'), (== '_') ] -- , (== ':'), (== '.') ]

tshow :: Show a => a -> Text
tshow = Text.pack . show

ullist :: [UI a] -> UI a
ullist = UL .  map LI

selector :: (ToURL a, Show (URLT a)) => a -> Text -- Should be (Tagged JQSelector JExpr) eventually
selector url = '#' `cons` (clean url)

findId :: UI a -> Text
findId (ID s _) = s
findId _ = error $ "UI.findId expects the top element to be the ID tag."

e ! a = e (with [a])
dataAttribute n v = makeAttribute ("data-" <> n) v
toValue = id

jsToText :: (JMacro a, JsToDoc a) => a -> Text
jsToText = Text.pack . show . renderJs

  
renderHtml5 :: (ToURL a, Show (URLT a), ToJExpr (URLT a)) => UI a -> Html ()
renderHtml5 (Call url) = error $ "Calls should be replaced before rendering UIs:" ++  show (toURL url)
renderHtml5 (AjaxUpload url multiple divId fileTag namespace buttonText reloadParent) =
  ajaxFileUploadInit (show $ toURL url) multiple fileTag divId namespace buttonText reloadParent
renderHtml5 (FormMP url uis) = form_ [enctype_ "multipart/form-data", method_ "POST", action_ (tshow . toURL $ url) ] $ mapM_ renderHtml5 uis
renderHtml5 (Upload fileUploadTag js width) = input_ [type_ "file", name_ fileUploadTag
                                                     , multiple_ "", size_ (tshow width), onchange_ (jsToText js)]
renderHtml5 (Submit val) = input_  [type_ "submit", value_ val]
-- renderHtml5 (Escape h) = reindeer h
renderHtml5 (Span ui) = span_ $ renderHtml5 ui
renderHtml5 (Union us) = mapM_ renderHtml5 us
renderHtml5 HR = hr_ []
renderHtml5 BR = br_ []
renderHtml5 (UL us) = (ul_ . mapM_ renderHtml5) us
renderHtml5 (LI ui) = (li_ . renderHtml5) ui
renderHtml5 (Ref s ui) = renderHtml5 $ lookupID s ui
-- renderHtml5 (ID s ui) = div_ ! id_ s $ renderHtml5 ui  -- insertID s ui
renderHtml5 (ID s ui) = renderHtml5 ui `with` [id_ s]
renderHtml5 (Label s) = div_ [class_ "ui-label"] $ label_ (toHtml s)
renderHtml5 (Rabel s) = div_ [class_ "ui-label", style_ "text-align: right" ]$  label_ (toHtml s)
renderHtml5 (Title s) = title_ $ toHtml s
renderHtml5 (LabelRef labelUI targetUI) = div_ $ label_ [for_ (findId targetUI)] $ renderHtml5 labelUI
renderHtml5 (Att as ui) = renderHtml5 ui `with` map uiattr (consolidate as)
renderHtml5 (AttData key val ui) = renderHtml5 ui `with` [data_ key val]
renderHtml5 (Txt atts s) = span_ [ style_ (inlineCSS atts)] $ toHtml s
renderHtml5 (Txt1 s) = span_ $ toHtml s
-- renderHtml5 (NBTxt s) = sequence_ . intersperse (preEscapedToMarkup nbsp) . Prelude.map toHtml . words $ s
renderHtml5 (NBTxt s) = span_ [ style_ (inlineCSS [("white-space","nowrap")])] $ toHtml s
renderHtml5 (P s) = p_ $ toHtml s
renderHtml5 (Form uis) = form_ $ mapM_ renderHtml5 uis
renderHtml5 (FieldSet uis) = fieldset_ $ mapM_ renderHtml5 uis
renderHtml5 (Legend ui) = legend_ $ renderHtml5 ui
renderHtml5 (TTextFormW url arrow w ms) =  input_ [style_ (inlineCSS [("width",w)]), id_ ident,  type_ "text", value_ (maybe "" id ms),  g, s]
    where ident = clean url
          sel = Text.unpack $ '#' `cons` ident
          g = onchange_ (jsToText $ postS arrow (toURL url) thisValue oldValue)
          s = onfocus_ (jsToText $ unpostS arrow sel (toURL url))
renderHtml5 (TTTextFormW url arrow postJS w ms) =  input_ [ style_ (inlineCSS [("width",w)]), id_ ident, type_ "text", value_ (maybe "" id ms), g, s]
    where ident = clean url
          sel = Text.unpack $ '#' `cons` ident
          g = onchange_ (jsToText $ postSS arrow postJS (toURL url) thisValue oldValue)
          s = onfocus_ (jsToText $ unpostS arrow sel (toURL url))
renderHtml5 (TTextForm url arrow ms) = renderHtml5 $ TTextFormW url arrow "25em" ms
renderHtml5 (TTextAreaForm url arrow (r,c) ms) = textarea_ `with` [rs, cs, g, s] $ toHtml (maybe mempty id ms)
    where rs = rows_ (Text.pack $ show r)
          cs = cols_ (Text.pack $ show c)
          g = onchange_ (jsToText $ postS arrow (toURL url) thisValue oldValue)
          s = onfocus_ (jsToText $ unpostS arrow sel (toURL url))
          ident = clean url
          sel = Text.unpack $ '#' `cons` ident
renderHtml5 (DropDown url arrow values current) = select_ [g , s] $ sequence_ $ map fopt values
  where g = onchange_ (jsToText $ postS arrow (toURL url) thisValue oldValue)
        s = onfocus_ (jsToText $ unpostS arrow sel (toURL url))
        ident = clean url
        sel = Text.unpack $ '#' `cons` ident
        fopt (v,ui) =
          if (v == current)
          then option_ [ selected_ "selected", value_ v] $ renderHtml5 ui
          else option_ [ value_ v] $ renderHtml5 ui
renderHtml5 (ActionMenu menuName actions) = actionMenuMarkup menuName actions
{-
  where ch =
        ident = clean url
        sel = '#':ident
        fopt (v,ui) = option ! HA.value (toValue v) $ renderHtml5 ui
-}

{-
          g = onfocus (toValue $ download ("" :: Text) (toURL url))
          s = onchange (toValue $ upload ("" :: Text) (toURL url))
-}
renderHtml5 (GenericButton js ui) =  renderHtml5 (Att [(Class "genericButton")] ui) `with` [onclick_ (jsToText js)]
renderHtml5 (Button _url js ui) = div_ $ button_ [class_ "haskell", type_ "button", onclick_ (jsToText js)] $ renderHtml5 ui
renderHtml5 (ButtonRaw js ui) = button_ [type_ "button", onclick_ (jsToText js)] $ renderHtml5 ui
-- renderHtml5 (Button _url ui) = div_ $ renderHtml5 ui
renderHtml5 (Video url) = toHtml $ "Your video here:" <> (tshow $ toURL url)
renderHtml5 (Image url (ww,hh)) = img_ [ width_ (tshow ww), height_ (tshow hh), src_ (tshow $ toURL url)]
renderHtml5 (ImageUnbnd url) = img_ [src_ (tshow $ toURL url)]
renderHtml5 (Thumb url (ww,hh) althover) = img_ [width_ (tshow ww), height_ (tshow hh), src_ (tshow $ toURL url) , alt_ althover]
renderHtml5 (ThumbCSS url althover) = img_ [ src_ (tshow $ toURL url), alt_ althover]
renderHtml5 (Cropper url (iw,ih)) = img_ [src_ (tshow $ toURL url)
                                         , width_ $ tshow iw <> "px"
                                         , height_ $ tshow ih <> "px"
                                         ]
--renderHtml5 (Box uis) = table_ . tr_ . sequence_ $ P.map (td . renderHtml5) uis
--renderHtml5 (VBox uis) = table_ . sequence_ $ map (tr_ . td . renderHtml5) uis
renderHtml5 (Div ui) = div_ $ renderHtml5 ui
renderHtml5 (Box atts uis) = div_ [ style_ (inlineCSS atts), class_ "hbox"] $ mapM_ renderHtml5 uis
renderHtml5 (ZBox uis) = div_ [ class_ "zbox"] $ mapM_ renderHtml5 uis
renderHtml5 (VBox atts uis) = div_ [ style_ (inlineCSS atts), class_ "vbox"] $ mapM_ renderHtml5 uis
renderHtml5 (HBox atts uis) = div_ [ style_ (inlineCSS atts), class_ "hbox"] $ mapM_ renderHtml5 uis
renderHtml5 (VVBox agn uis) = div_ [ class_ ("vbox" `align` agn) ] $ sequence_ $ map renderHtml5 uis
renderHtml5 (HHBox agn uis) = div_ [ class_ ("hbox" `align` agn)] $ sequence_ $ map (div_ . renderHtml5) uis
renderHtml5 (IFrame url) = iframe_ [ src_ (tshow $ toURL url)] $ return ()
renderHtml5 (CheckBox bb) = input_ [type_ "checkbox", value_ (tshow bb)]
renderHtml5 (CheckBoxM Nothing) = input_ [type_ "checkbox"]
renderHtml5 (CheckBoxM (Just bb)) = input_ [type_ "checkbox", value_ (tshow bb)]
renderHtml5 (RadioButton nm val) = div_ $ (input_ [type_ "radio", name_ nm, value_ val])  -- : (map (td . renderHtml5) uis)
renderHtml5 (Glue glue u) = div_ [ class_ "spacer", css "width" (tshow glue) u] $ toHtmlRaw nbsp
renderHtml5 (Spacer w) = div_ [ style_ ("width: " <> w)] $ toHtmlRaw nbsp
renderHtml5 (MinMax mn mx) = div_ [ style_ ("min-width: " <> mn <> "; " <> "max-width: " <> mx)] $ toHtmlRaw nbsp
renderHtml5 (Editor) = textarea_ "Edit me!"
renderHtml5 (CSSAttr prs ui) = div_ [style_ (toValue (inlineCSS prs))] $ renderHtml5 ui
renderHtml5 (CSSAttrI prs ui) =  renderHtml5 ui `with` [style_ $ inlineCSS prs]
renderHtml5 (DynTabSet side tabs) = renderHtml5 tabs'
  where tabs' = Att [Class cname] $ Div $ UL $ map (LI . fink) tabs
        fink (lab, url) = Link url lab
        cname :: Text
        cname = "jquerytabs" <> vertical
        vertical = if side == LeftSide then " tabs-left" else ""
renderHtml5 (TabSet side nm uis) = div_ [class_ ("jquerytabs" <> vertical)] $ sequence_ ( tabs : bodies)
    where tabs = ul_ $ sequence_ $ map (\(ref,u) -> li_ . (a_ [href_ ref]) . xxx . fst $ u) (zip refs uis)
          bodies = map (\(anch,u) -> (div_ [id_ anch]) . renderHtml5 . snd $ u) (zip anchors uis)
          refs = map ('#' `cons`) ids
          anchors = map toValue ids
          ids :: [Text]
          ids = map (\n -> nm <> "-" <> tshow n) ([1..] :: [Int])
          vertical = if side == LeftSide then " tabs-left" else ""
          xxx = renderHtml5
          -- xxx (Label s) = toHtml s
          -- xxx _ = error "UI xxx hack failed."
{-
renderHtml5 (TabSet _side _nm uis) = div_ ! class_ "jquerytabs"  $ ul_ $ sequence_ $ map (li_) $ (map (renderHtml5 . fst) uis)
table_ . sequence_ $ map (tr_ . td_) $
                                     (menu $ sequence_ $ map li_ (map (renderHtml5 . fst) uis)) :
                                     (map (renderHtml5 . snd) uis)
-}


renderHtml5 (TabSelect TopSide _nm tabs ui) = renderHtml5 $ VBox [] [ HBox [] (Glue 1.5 "em" : (intersperse (Glue 1.5 "em") (map (Background "#DDDDDD") tabs)))
                                                                    , ui
                                                                    ]
renderHtml5 (TabSelect LeftSide _nm tabs ui) = renderHtml5 $ HBox [] [ VBox [] ((Spacer "1em"):(intersperse (Spacer "1em") (map (Background "#DDDDDD") tabs)))
                                                                     , ui
                                                                     ]
renderHtml5 (ToDo ui) = renderHtml5 ui
renderHtml5 (Background color ui) = div_ [css "background-color" color ""] $ renderHtml5 ui
renderHtml5 (FontSize sz ui) = renderHtml5 ui `with` [css "font-size" (tshow sz) "pt"]
renderHtml5 (TableOnly) = table_ $ ""
renderHtml5 (Table atts hs uis) = table_ [style_ (inlineCSS atts)] $
                                  do (thead_ . tr_) (mapM_ (td_  . renderHtml5) hs)
                                     mapM_ (tr_ . mapM_ (td_ . renderHtml5)) uis
renderHtml5 (Clients atts hs uis) = table_ [style_  (inlineCSS atts)] $
                                    do (thead_ . tr_) (mapM_ (td_  . renderHtml5) hs)
                                       mapM_ (tr_ . mapM_ (td_ . renderHtml5)) uis
renderHtml5 (Link url ui) = a_ [href_ (tshow $ toURL url)] $ renderHtml5 ui
-- For LinkTab, the _blank attribute is magic that tells a browser to open a link in a new tab.  This should probably be an application setting.
-- renderHtml5 (LinkTab url ui) = a_ ! href (toValue (show $ toURL url)) ! HA.target (toValue ("_blank" :: Text)) $ renderHtml5 ui
renderHtml5 (LinkTab url ui) = a_ [href_ (tshow $ toURL url)] $ renderHtml5 ui
renderHtml5 (Anchor url ui) = a_ [ name_ (tshow $ toURL url)] $ renderHtml5 ui
renderHtml5 (SlideOut url ui) = div_ [ id_ ident]  $ renderHtml5 ui
    where ident = clean url
          -- sel = '#':ident


printHtml5 :: (ToURL a, Show (URLT a), ToJExpr (URLT a)) => UI a -> IO ()
printHtml5 = print . renderHtml5

balance :: (Data a, ToURL a) => UI a -> UI a
balance = transform bal
    where bal x = CSSAttr [("width",tshow (uiwidth x))] x


css :: CSSField -> Text -> CSSUnits -> Attribute
css field val units = style_ $ toValue $ Text.concat [showcss field,": ",val,units]



tabSelect :: (ToURL url) => (Text, Text) -> UI url -> UI url
tabSelect pr (Box atts uis) = Box atts $ map (tabSelect pr) uis
tabSelect pr (VBox atts uis) = VBox atts $ map (tabSelect pr) uis
tabSelect pr (VVBox agn uis) = VVBox agn $ map (tabSelect pr) uis
tabSelect pr (HBox atts uis) = HBox atts $ map (tabSelect pr) uis
tabSelect pr (HHBox agn uis) = HHBox agn $ map (tabSelect pr) uis
tabSelect pr (Link url ui) = Link url $ (tabSelect pr) ui
tabSelect pr (ToDo ui) = ToDo $ (tabSelect pr) ui
tabSelect pr (FontSize sz ui) = FontSize sz $ (tabSelect pr) ui
tabSelect pr (Background c ui) = Background c $ (tabSelect pr) ui
tabSelect pr@(set,tab) (TabSet side nm tabs) | nm == set = TabSelect side nm (fst $ unzip tabs) $ findTab tab tabs
                                             | otherwise = TabSet side nm $ map (second (\ui -> tabSelect pr ui)) tabs
tabSelect _ ui = ui

findTab :: ToURL a => Text -> [(UI a, UI a)] -> UI a
findTab _tabname [] = error "UI.hs findTab: Tab not found"
findTab tabname ((ID n _, tui):uis) | n == tabname = tui
                                    | otherwise = findTab tabname uis
findTab _ _  = error "UI.hs findTab, ID not at top of tab"

type CSSField = Text
type CSSUnits = Text
showcss :: a -> a
showcss = id

maxi :: [Int] -> Int
maxi = foldr max 0

maxf :: [Float] -> Float
maxf = foldr max 0.0

treedepth :: Data a => UI a -> Int
treedepth x = 1 + maxi (map treedepth (children x))

uiwidth :: Data a => UI a -> Float
uiwidth (Label s) = ems s
uiwidth (Rabel s) = ems s
uiwidth (Union xs) = maxf (map uiwidth xs)
uiwidth (Txt _ s) = ems s -- Txt is like label or text input, one line, not wrapped.
uiwidth (NBTxt s) = ems s
uiwidth (P _s) = 0.0  -- ???   can be wrapped arbitrarily small?  Seems wrong.  Function?
uiwidth (Image _ (iw,_)) = fromIntegral iw
uiwidth (Cropper _ (w,_)) =  fromIntegral  w
uiwidth (HHBox _ xs) = sum (map uiwidth xs)
uiwidth (VVBox _ xs) = maxf (map uiwidth xs)
uiwidth (Glue w _) = w
uiwidth (TTextForm _ _ def) = maybe 25.0 ems def
uiwidth (TTextFormW _ _ w _) = fooWidth w
uiwidth (GenericButton _ ui) = uiwidth ui
uiwidth x = sum (map uiwidth (children x))

uiheight :: Data a => UI a -> Float
uiheight (Label _s) = 1.0
uiheight (Rabel _s) = 1.0
uiheight (Union xs) = maxf (map uiheight xs)
uiheight (Txt _ _s) = 1.0 -- Txt is like label or text input, one line, not wrapped.
uiheight (P _s) = 0.0  -- ???   can be wrapped arbitrarily small?  Seems wrong.  Function?
uiheight (Image _ (w,_)) = fromIntegral w
uiheight (Cropper _ (w,_)) =  fromIntegral w
uiheight (HHBox _ xs) = maxf (map uiheight xs)
uiheight (VVBox _ xs) = sum (map uiheight xs)
uiheight (Glue w _) = w
uiheight (GenericButton _ ui) = uiheight ui
uiheight x = sum (map uiheight (children x))

ems :: Text -> Float
ems = (0.6 *) . fromIntegral . Text.length


fooWidth :: (Fractional a, Read a) => Text -> a
fooWidth ws =
    case break (not . isDigit) (Text.unpack ws) of
      ("", _) -> 0.0
      (x, "") -> read x
      (x,"em") -> read x
      (x,_) -> read x

truncateW :: Dimension -> UI url -> UI url
truncateW w = Att [Class "truncate", Style . CSS $ formatCSS [("width",reindeer w)]]


-- maxwidth, maxheight :: AttributeValue -> Attribute
-- maxwidth = customAttribute "max-width"
-- maxheight = customAttribute "max-height"
