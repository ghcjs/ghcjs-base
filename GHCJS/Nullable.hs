{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHCJS.Nullable ( Nullable(..)
                      , nullableToMaybe
                      , maybeToNullable
                      ) where

import Data.Typeable (Typeable)
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import GHCJS.Marshal.Maybe
import GHCJS.Prim (JSVal(..))
import GHCJS.Types (IsJSVal)

newtype Nullable a = Nullable JSVal
  deriving (Typeable, IsJSVal)

nullableToMaybe :: (PFromJSVal a, EnsureNotMaybe a) => Nullable a -> Maybe a
nullableToMaybe (Nullable r) = pFromJSVal r
{-# INLINE nullableToMaybe #-}

maybeToNullable :: (PToJSVal a, EnsureNotMaybe a) => Maybe a -> Nullable a
maybeToNullable = Nullable . pToJSVal
{-# INLINE maybeToNullable #-}
