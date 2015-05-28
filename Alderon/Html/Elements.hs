{-# LANGUAGE OverloadedStrings #-}

module Alderon.Html.Elements where
import Alderon.Html.Internal

a_ :: Html -> Html
a_ = parent "a"

abbr_ :: Html -> Html
abbr_ = parent "abbr"

address_ :: Html -> Html
address_ = parent "address"

article_ :: Html -> Html
article_ = parent "article"

aside_ :: Html -> Html
aside_ = parent "aside"

audio_ :: Html -> Html
audio_ = parent "audio"

b_ :: Html -> Html
b_ = parent "b"

bdi_ :: Html -> Html
bdi_ = parent "bdi"

bdo_ :: Html -> Html
bdo_ = parent "bdo"

big_ :: Html -> Html
big_ = parent "big"

blockquote_ :: Html -> Html
blockquote_ = parent "blockquote"

body_ :: Html -> Html
body_ = parent "body"

button_ :: Html -> Html
button_ = parent "button"

canvas_ :: Html -> Html
canvas_ = parent "canvas"

caption_ :: Html -> Html
caption_ = parent "caption"

cite_ :: Html -> Html
cite_ = parent "cite"

code_ :: Html -> Html
code_ = parent "code"

colgroup_ :: Html -> Html
colgroup_ = parent "colgroup"

data_ :: Html -> Html
data_ = parent "data"

datalist_ :: Html -> Html
datalist_ = parent "datalist"

dd_ :: Html -> Html
dd_ = parent "dd"

del_ :: Html -> Html
del_ = parent "del"

details_ :: Html -> Html
details_ = parent "details"

dfn_ :: Html -> Html
dfn_ = parent "dfn"

div_ :: Html -> Html
div_ = parent "div"

dl_ :: Html -> Html
dl_ = parent "dl"

dt_ :: Html -> Html
dt_ = parent "dt"

em_ :: Html -> Html
em_ = parent "em"

fieldset_ :: Html -> Html
fieldset_ = parent "fieldset"

figcaption_ :: Html -> Html
figcaption_ = parent "figcaption"

figure_ :: Html -> Html
figure_ = parent "figure"

footer_ :: Html -> Html
footer_ = parent "footer"

form_ :: Html -> Html
form_ = parent "form"

h1_ :: Html -> Html
h1_ = parent "h1"

h2_ :: Html -> Html
h2_ = parent "h2"

h3_ :: Html -> Html
h3_ = parent "h3"

h4_ :: Html -> Html
h4_ = parent "h4"

h5_ :: Html -> Html
h5_ = parent "h5"

h6_ :: Html -> Html
h6_ = parent "h6"

head_ :: Html -> Html
head_ = parent "head"

header_ :: Html -> Html
header_ = parent "header"

html_ :: Html -> Html
html_ = parent "html"

i_ :: Html -> Html
i_ = parent "i"

iframe_ :: Html -> Html
iframe_ = parent "iframe"

input_ :: Html -> Html
input_ = parent "input"

ins_ :: Html -> Html
ins_ = parent "ins"

kbd_ :: Html -> Html
kbd_ = parent "kbd"

label_ :: Html -> Html
label_ = parent "label"

legend_ :: Html -> Html
legend_ = parent "legend"

li_ :: Html -> Html
li_ = parent "li"

main_ :: Html -> Html
main_ = parent "main"

map_ :: Html -> Html
map_ = parent "map"

mark_ :: Html -> Html
mark_ = parent "mark"

menu_ :: Html -> Html
menu_ = parent "menu"

menuitem_ :: Html -> Html
menuitem_ = parent "menuitem"

meter_ :: Html -> Html
meter_ = parent "meter"

nav_ :: Html -> Html
nav_ = parent "nav"

noscript_ :: Html -> Html
noscript_ = parent "noscript"

object_ :: Html -> Html
object_ = parent "object"

ol_ :: Html -> Html
ol_ = parent "ol"

optgroup_ :: Html -> Html
optgroup_ = parent "optgroup"

option_ :: Html -> Html
option_ = parent "option"

output_ :: Html -> Html
output_ = parent "output"

p_ :: Html -> Html
p_ = parent "p"

pre_ :: Html -> Html
pre_ = parent "pre"

progress_ :: Html -> Html
progress_ = parent "progress"

q_ :: Html -> Html
q_ = parent "q"

rp_ :: Html -> Html
rp_ = parent "rp"

rt_ :: Html -> Html
rt_ = parent "rt"

ruby_ :: Html -> Html
ruby_ = parent "ruby"

s_ :: Html -> Html
s_ = parent "s"

samp_ :: Html -> Html
samp_ = parent "samp"

script_ :: Html -> Html
script_ = parent "script"

section_ :: Html -> Html
section_ = parent "section"

select_ :: Html -> Html
select_ = parent "select"

small_ :: Html -> Html
small_ = parent "small"

span_ :: Html -> Html
span_ = parent "span"

strong_ :: Html -> Html
strong_ = parent "strong"

style_ :: Html -> Html
style_ = parent "style"

sub_ :: Html -> Html
sub_ = parent "sub"

summary_ :: Html -> Html
summary_ = parent "summary"

sup_ :: Html -> Html
sup_ = parent "sup"

table_ :: Html -> Html
table_ = parent "table"

tbody_ :: Html -> Html
tbody_ = parent "tbody"

td_ :: Html -> Html
td_ = parent "td"

textarea_ :: Html -> Html
textarea_ = parent "textarea"

tfoot_ :: Html -> Html
tfoot_ = parent "tfoot"

th_ :: Html -> Html
th_ = parent "th"

thead_ :: Html -> Html
thead_ = parent "thead"

time_ :: Html -> Html
time_ = parent "time"

title_ :: Html -> Html
title_ = parent "title"

tr_ :: Html -> Html
tr_ = parent "tr"

u_ :: Html -> Html
u_ = parent "u"

ul_ :: Html -> Html
ul_ = parent "ul"

var_ :: Html -> Html
var_ = parent "var"

video_ :: Html -> Html
video_ = parent "video"

area_ :: Html
area_ = leaf "area"

base_ :: Html
base_ = leaf "base"

br_ :: Html
br_ = leaf "br"

col_ :: Html
col_ = leaf "col"

command_ :: Html
command_ = leaf "command"

embed_ :: Html
embed_ = leaf "embed"

hr_ :: Html
hr_ = leaf "hr"

img_ :: Html
img_ = leaf "img"

keygen_ :: Html
keygen_ = leaf "keygen"

link_ :: Html
link_ = leaf "link"

meta_ :: Html
meta_ = leaf "meta"

param_ :: Html
param_ = leaf "param"

source_ :: Html
source_ = leaf "source"

track_ :: Html
track_ = leaf "track"

wbr_ :: Html
wbr_ = leaf "wbr"
