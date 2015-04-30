{-# LANGUAGE OverloadedStrings #-}

module Alderon.Html.Attributes ( ) where
import Alderon.Html.Internal
import Data.Text (Text)

accept_ :: Text -> Attribute
accept_ = attribute "accept"

accessKey_ :: Text -> Attribute
accessKey_ = attribute "accessKey"

action_ :: Text -> Attribute
action_ = attribute "action"

allowFullScreen_ :: Text -> Attribute
allowFullScreen_ = attribute "allowFullScreen"

allowTransparency_ :: Text -> Attribute
allowTransparency_ = attribute "allowTransparency"

alt_ :: Text -> Attribute
alt_ = attribute "alt"

cellPadding_ :: Text -> Attribute
cellPadding_ = attribute "cellPadding"

cellSpacing_ :: Text -> Attribute
cellSpacing_ = attribute "cellSpacing"

charset_ :: Text -> Attribute
charset_ = attribute "charset"

colSpan_ :: Text -> Attribute
colSpan_ = attribute "colSpan"

cols_ :: Text -> Attribute
cols_ = attribute "cols"

content_ :: Text -> Attribute
content_ = attribute "content"

contentEditable_ :: Text -> Attribute
contentEditable_ = attribute "contentEditable"

contextMenu_ :: Text -> Attribute
contextMenu_ = attribute "contextMenu"

controls_ :: Text -> Attribute
controls_ = attribute "controls"

data_ :: Text -> Attribute
data_ = attribute "data"

dateTime_ :: Text -> Attribute
dateTime_ = attribute "dateTime"

dir_ :: Text -> Attribute
dir_ = attribute "dir"

draggable_ :: Text -> Attribute
draggable_ = attribute "draggable"

encType_ :: Text -> Attribute
encType_ = attribute "encType"

for_ :: Text -> Attribute
for_ = attribute "for"

form_ :: Text -> Attribute
form_ = attribute "form"

formNoValidate_ :: Text -> Attribute
formNoValidate_ = attribute "formNoValidate"

frameBorder_ :: Text -> Attribute
frameBorder_ = attribute "frameBorder"

height_ :: Text -> Attribute
height_ = attribute "height"

href_ :: Text -> Attribute
href_ = attribute "href"

htmlFor_ :: Text -> Attribute
htmlFor_ = attribute "htmlFor"

icon_ :: Text -> Attribute
icon_ = attribute "icon"

label_ :: Text -> Attribute
label_ = attribute "label"

lang_ :: Text -> Attribute
lang_ = attribute "lang"

list_ :: Text -> Attribute
list_ = attribute "list"

max_ :: Text -> Attribute
max_ = attribute "max"

maxLength_ :: Text -> Attribute
maxLength_ = attribute "maxLength"

method_ :: Text -> Attribute
method_ = attribute "method"

min_ :: Text -> Attribute
min_ = attribute "min"

name_ :: Text -> Attribute
name_ = attribute "name"

pattern_ :: Text -> Attribute
pattern_ = attribute "pattern"

placeholder_ :: Text -> Attribute
placeholder_ = attribute "placeholder"

poster_ :: Text -> Attribute
poster_ = attribute "poster"

radioGroup_ :: Text -> Attribute
radioGroup_ = attribute "radioGroup"

rel_ :: Text -> Attribute
rel_ = attribute "rel"

role_ :: Text -> Attribute
role_ = attribute "role"

rowSpan_ :: Text -> Attribute
rowSpan_ = attribute "rowSpan"

rows_ :: Text -> Attribute
rows_ = attribute "rows"

sandbox_ :: Text -> Attribute
sandbox_ = attribute "sandbox"

scope_ :: Text -> Attribute
scope_ = attribute "scope"

scrollLeft_ :: Text -> Attribute
scrollLeft_ = attribute "scrollLeft"

scrollTop_ :: Text -> Attribute
scrollTop_ = attribute "scrollTop"

selected_ :: Text -> Attribute
selected_ = attribute "selected"

size_ :: Text -> Attribute
size_ = attribute "size"

span_ :: Text -> Attribute
span_ = attribute "span"

src_ :: Text -> Attribute
src_ = attribute "src"

srcDoc_ :: Text -> Attribute
srcDoc_ = attribute "srcDoc"

step_ :: Text -> Attribute
step_ = attribute "step"

style_ :: Text -> Attribute
style_ = attribute "style"

tabIndex_ :: Text -> Attribute
tabIndex_ = attribute "tabIndex"

target_ :: Text -> Attribute
target_ = attribute "target"

title_ :: Text -> Attribute
title_ = attribute "title"

type_ :: Text -> Attribute
type_ = attribute "type"

value_ :: Text -> Attribute
value_ = attribute "value"

width_ :: Text -> Attribute
width_ = attribute "width"

wmode_ :: Text -> Attribute
wmode_ = attribute "wmode"

async_ :: Attribute
async_ = boolean "async"

autocomplete_ :: Attribute
autocomplete_ = boolean "autocomplete"

autofocus_ :: Attribute
autofocus_ = boolean "autofocus"

autoplay_ :: Attribute
autoplay_ = boolean "autoplay"

checked_ :: Attribute
checked_ = boolean "checked"

defer_ :: Attribute
defer_ = boolean "defer"

disabled_ :: Attribute
disabled_ = boolean "disabled"

download_ :: Attribute
download_ = boolean "download"

hidden_ :: Attribute
hidden_ = boolean "hidden"

loop_ :: Attribute
loop_ = boolean "loop"

multiple_ :: Attribute
multiple_ = boolean "multiple"

noValidate_ :: Attribute
noValidate_ = boolean "noValidate"

preload_ :: Attribute
preload_ = boolean "preload"

readOnly_ :: Attribute
readOnly_ = boolean "readOnly"

required_ :: Attribute
required_ = boolean "required"

reversed_ :: Attribute
reversed_ = boolean "reversed"

seamless_ :: Attribute
seamless_ = boolean "seamless"

spellCheck_ :: Attribute
spellCheck_ = boolean "spellCheck"
