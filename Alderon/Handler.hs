{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Alderon.Handler where

import Alderon.Html.Internal
import Alderon.Html.Events   as E
import Data.Text as Text (Text, toLower, pack)

default (Text)

eventName :: EventType -> Text
eventName eventType = case eventType of
    DoubleClick -> "dblclick"
    MouseEnter  -> "mouseover"
    MouseLeave  -> "mouseout"
    _           -> Text.toLower . Text.pack $ show eventType

-- | Set the handler for the @Focus@ event.
onFocus :: Handler f => f E.FocusEvent -> Attribute
onFocus = onEvent Focus

-- | Set the handler for the @Blur@ event.
onBlur :: Handler f => f E.BlurEvent -> Attribute
onBlur = onEvent Blur


-- FocusIn/Out not widely implemented.
--    -- | Set the handler for the @FocusIn@ event.
-- onFocusIn :: Handler f => f E.FocusEvent -> Attribute
-- onFocusIn = onEvent FocusIn

--  -- | Set the handler for the @FocusOut@ event.
-- onFocusOut :: Handler f => f E.FocusEvent -> Attribute
-- onFocusOut = onEvent FocusOut

-- | Set the handler for the @Input@ event.
onInput :: Handler f => f E.InputEvent -> Attribute
onInput = onEvent Input

-- | Set the handler for the @KeyDown@ event.
onKeyDown :: Handler f => f E.KeyboardEvent -> Attribute
onKeyDown = onEvent KeyDown

-- | Set the handler for the @KeyPress@ event.
onKeyPress :: Handler f => f E.KeyboardEvent -> Attribute
onKeyPress = onEvent KeyPress

-- | Set the handler for the @KeyUp@ event.
onKeyUp :: Handler f => f E.KeyboardEvent -> Attribute
onKeyUp = onEvent KeyUp

-- | Set the handler for the @MouseDown@ event.
onMouseDown :: Handler f => f E.MouseEvent -> Attribute
onMouseDown = onEvent MouseDown

-- | Set the handler for the @MouseEnter@ event.
onMouseEnter :: Handler f => f E.MouseEvent -> Attribute
onMouseEnter = onEvent MouseEnter

-- | Set the handler for the @MouseLeave@ event.
onMouseLeave :: Handler f => f E.MouseEvent -> Attribute
onMouseLeave = onEvent MouseLeave

-- | Set the handler for the @MouseMove@ event.
onMouseMove :: Handler f => f E.MouseEvent -> Attribute
onMouseMove = onEvent MouseMove

-- | Set the handler for the @MouseUp@ event.
onMouseUp :: Handler f => f E.MouseEvent -> Attribute
onMouseUp = onEvent MouseUp

-- | Set the handler for the @Submit@ event.
onSubmit :: Handler f => f E.SubmitEvent -> Attribute
onSubmit = onEvent Submit

