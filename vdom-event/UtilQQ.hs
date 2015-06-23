{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module UtilQQ where

import System.IO (fixIO)
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack)

import GHCJS.Foreign (release, syncCallback, ForeignRetention(AlwaysRetain), toJSString)
import GHCJS.Foreign.QQ (js,js_)
import GHCJS.VDOM as VDOM (DOMNode, VNode, Properties, Children, text, vnode, noProps, mkChildren)
import GHCJS.VDOM.QQ (ch,pr)


textshow :: Show a => a -> Text
textshow = Text.pack . show

mkRoot :: IO DOMNode
mkRoot = do
  root <- documentCreateElement "div"
  documentBodyAppendChild root
  return root

documentCreateElement :: Text -> IO DOMNode
documentCreateElement tag = [js| document.createElement(`jstag) |]
  where jstag = toJSString tag

documentBodyAppendChild :: DOMNode -> IO ()
documentBodyAppendChild node = [js_| document.body.appendChild(`node); |]

atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- fixIO $ \cb ->
    syncCallback AlwaysRetain False (release cb >> m)
  [js_| window.requestAnimationFrame(`cb); |]

($$) :: (Children -> VNode) -> [VNode] -> VNode
node $$ cs = node (mkChildren cs)

textt :: Text -> VNode
textt = text . toJSString

textDiv :: Show a => a -> VNode
textDiv x = div_ noProps [ch|c|]
  where
    c = text . toJSString . show $ x

cls :: Text -> Properties
cls name = [pr| className: jsname |]
  where jsname = toJSString name

id_ :: Text -> Properties
id_ name = [pr| id: jsname |]
  where jsname = toJSString name


{-# INLINE h3_ #-}
h3_ :: Properties -> Children -> VNode
h3_ = vnode "h3"

{-# INLINE div_ #-}
div_ :: Properties -> Children -> VNode
div_ = vnode "div"

button_ :: Properties -> Children -> VNode
button_ = vnode "button"

label_ :: Properties -> Children -> VNode
label_ = vnode "label"

byId :: Text -> Text
byId = (Text.pack "#" <>)

