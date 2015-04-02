module GHCJS.Foreign.Callback.Internal where

import GHCJS.Types

newtype Callback a = Callback (JSRef ())
