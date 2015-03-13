{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Text
import Lucid

-- title :: (ToHtml s, Term (m a) b) => s -> m a -> b
-- title s m =
--   div_ $ do
--     h2_ $ s
--     m


-- title :: (Term arg result, Term s result) => s -> arg -> result
title :: Monad m => Text -> HtmlT m () -> HtmlT m ()
title s m = div_ $ do
  h2_ $ toHtml s
  m
