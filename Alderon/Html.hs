module Alderon.Html
    ( Html
    , text_
    , Handler(..)
    , Attribute
    , Attributable
    , (!)
    , (!?)
    , (!#)
    , (!.)
    , (!?.)
    , module Alderon.Html.Elements
    , module Alderon.Html.Attributes
    , module Alderon.Handler
    ) where

import Alderon.Html.Internal
import Alderon.Html.Elements hiding (data_)
import Alderon.Html.Attributes hiding (span_, form_, label_, style_, title_)
import Alderon.Handler
import Data.Tree


merge :: Html -> Html -> Html
merge a b = b
            -- do
  -- a' <- runHtml a
  -- b' <- runHtml b
  -- return $ _mergePure a' b'

--mergePure :: Forest Node -> Forest Node -> Forest Node
--mergePure as bs = zipWith

-- fail if differing tags?
-- really, I want a tree of tags, merging in trees of attributes, then applying to content.

