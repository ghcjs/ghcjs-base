module GHCJS.Nullable ( Nullable(..)
                      , nullableToMaybe
                      , maybeToNullable
                      ) where

import GHCJS.Prim (JSRef(..))
import GHCJS.Marshal.Pure (PToJSRef(..), PFromJSRef(..))

newtype Nullable a = Nullable JSRef

nullableToMaybe :: PFromJSRef a => Nullable a -> Maybe a
nullableToMaybe (Nullable r) = pFromJSRef r
{-# INLINE nullableToMaybe #-}

maybeToNullable :: PToJSRef a => Maybe a -> Nullable a
maybeToNullable = Nullable . pToJSRef
{-# INLINE maybeToNullable #-}


