{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Default
import Data.Text (Text)
import JavaScript.JQuery

default (Text)

main = do
  body <- select "body"
  button <- select "<div>Should do nothing</div>"
  response <- select "<div>Ugh, it did something.</div>"
  appendJQuery button body
  detach <- click (\e -> void $ appendJQuery response body) def button
  detach
  return ()

