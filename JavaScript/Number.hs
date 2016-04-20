module JavaScript.Number where

import GHCJS.Types

newtype Number = Number JSVal
instance IsJSVal Number
