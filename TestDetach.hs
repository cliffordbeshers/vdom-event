{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Default
import Data.Text (Text)
import JavaScript.JQuery

default (Text)

main = do
  body <- select "body"
  buttonDo <- select "<button>Should do something visible</button>"
  buttonDont <- select "<button>Should disable other button</button>"
  appendJQuery buttonDo body
  appendJQuery buttonDont body
  detachDo <- click (\e -> void (select "<div>Ugh, it did something.</div>" >>= flip appendJQuery body)) def buttonDo
  click (\e -> detachDo) def buttonDont
  return ()

