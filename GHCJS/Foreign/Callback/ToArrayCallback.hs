{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module GHCJS.Foreign.Callback.ToArrayCallback
  ( ToArrayCallback, toArrayCallback ) where

import GHCJS.Types (JSVal, IsJSVal)
import Unsafe.Coerce

class ToArrayCallback r s | r -> s where
  toArrayCallback :: r -> [JSVal] -> IO s

instance {-# OVERLAPPABLE #-} ToArrayCallback (IO r) r where
  toArrayCallback x [] = x
  toArrayCallback _ _ = error "toArrayCallback: Invalid number of arguments"

instance {-# OVERLAPPING #-} (IsJSVal j, ToArrayCallback r y) =>
  ToArrayCallback (j -> r) y where
  toArrayCallback f (x:xs) = toArrayCallback (f $ unsafeCoerce x) xs
  toArrayCallback _ _ = error "toArrayCallback: Invalid number of arguments"
