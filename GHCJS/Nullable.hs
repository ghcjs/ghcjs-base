module GHCJS.Nullable ( Nullable(..)
                      , nullableToMaybe
                      , maybeToNullable
                      ) where

import GHCJS.Foreign (isTruthy)
import GHCJS.Prim (JSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))

newtype Nullable a = Nullable JSVal

nullableToMaybe :: PFromJSVal a => Nullable a -> Maybe a
nullableToMaybe (Nullable r) = if (isTruthy r)
                               then Just $ pFromJSVal r
                               else Nothing
{-# INLINE nullableToMaybe #-}

maybeToNullable :: PToJSVal a => Maybe a -> Nullable a
maybeToNullable = Nullable . pToJSVal
{-# INLINE maybeToNullable #-}


