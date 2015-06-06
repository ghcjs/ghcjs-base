module GHCJS.Foreign.Callback.Internal where

import GHCJS.Types
import GHCJS.Marshal.Internal

newtype Callback a = Callback (JSRef ())

instance PToJSRef (Callback a) where
  pToJSRef (Callback x) = castRef x

instance ToJSRef (Callback a) where
  toJSRef               = toJSRef_pure
