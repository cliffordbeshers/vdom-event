{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module AppraisalScribe.Reindeer where

-- Truth is, I don't really know what I want from this class.
-- I want it to make command line testing easier.
-- I want to avoid looking up render functions.

-- 1) The output should be an instance of Show, but for purdy, should
-- display really nicely, not encoded. E.g., for html, it shuld have
-- line breaks and only internal strings should be escaped.

-- is there a unifying class for Strings?

import Data.Monoid

class (Monoid b) => Reindeer a b where
    reindeer :: a -> b
    purdy :: a -> b
    purdy = reindeer

instance (Reindeer a b) => Reindeer [a] b where
    reindeer = foldr (mappend . reindeer) mempty
