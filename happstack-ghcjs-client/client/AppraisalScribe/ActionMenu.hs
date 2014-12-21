{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module AppraisalScribe.ActionMenu where

import Language.Javascript.JMacro

import Lucid

import AppraisalScribe.JMacro ()
import Text.JSON.Generic
import Data.Text as Text (Text, pack)

actionMenuJS :: JStat
actionMenuJS =
  [jmacro|
       function !menuReset(menu,v) {
          menu.children[0].removeAttribute("disabled");
          menu.children[0].label=v;
          menu.children[0].innerText=v;
          menu.value = v;
       }
       function !menuSelect(menu,v,action) {
          action();
          menuReset(menu,v);
       }
       function !menuActivate(menu) {
          menu.children[0].label="";
          menu.children[0].innerText="";
          menu.children[0].setAttribute("disabled","");
       }
  |]

actionMenuMarkup :: Text -> [(Text,JStat)] -> Html ()
actionMenuMarkup nm pairs =
  select_ [ onmousedown_ (js [jmacro|menuActivate(this);|])
          , onmouseup_ (js [jmacro|menuReset(this,`(nm)`);|])
          , onchange_ (js [jmacro|menuSelect(this,`(nm)`,`(toJSON pairs)`);|])
          ]
  $ sequence_ options
  where options :: [Html ()]
        options = (option_ $ toHtml nm ) : (map mkActions pairs)
        mkActions :: (Text,JStat) -> Html ()
        mkActions (s,u) = option_ [ value_ (js u), label_ s] $ toHtml s
        js = Text.pack . show . renderJs

