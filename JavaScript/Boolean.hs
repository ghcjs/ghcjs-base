module JavaScript.Boolean where

import GHCJS.Types

newtype Boolean = Boolean JSVal
instance IsJSVal Boolean
