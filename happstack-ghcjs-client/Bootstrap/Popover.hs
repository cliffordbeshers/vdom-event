{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Clckwrks.AVR3.Bootstrap.Popover where

import Prelude
import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.Javascript.JMacro

import JQuerySelector
import Clckwrks.AVR3.Bootstrap.Blaze
import Data.Aeson
import Data.Aeson.TH

data Compass = North | East | South | West 
             deriving (Eq, Ord, Enum, Read, Show, Typeable, Data)
                               
data Placement = Top | Bottom | Left | Right

data Trigger = Click | Hover | Focus | Manual

data PopoverOptions = PopoverOptions { placement :: Placement
                                     , trigger :: Trigger
                                     }
$(deriveJSON defaultOptions ''Placement)
$(deriveJSON defaultOptions ''Trigger)
$(deriveJSON defaultOptions ''PopoverOptions)


  
-- http://getbootstrap.com/javascript/#popovers
-- When using popovers on elements within a .btn-group or an
-- .input-group, you'll have to specify the option container: 'body'
-- (documented below) to avoid unwanted side effects (such as the
-- element growing wider and/or losing its rounded corners when the
-- popover is triggered).

popover :: JSelector -> PopoverOptions -> Markup -> JStat
popover sel options content = [jmacro|$(`(sel)`).popover(`(show $ toJSON options')`);|]
  where options' = f options content
        f _x _y = ()
-- placement: auto %s
-- trigger : show t
