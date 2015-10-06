{-# LANGUAGE TypeFamilies,
             DataKinds,
             PolyKinds,
             UndecidableInstances
  #-}

-- | This module implements instances of 'ToJS' and 'FromJS' for
-- 'Maybe'. @(Maybe a)@ marshals to and from @(Nullable (JSType a))@.
-- 'Nothing' becomes 'jsNull', whereas 'Just' uses the marshaling for
-- @a@. Some type system hackery is used to ensure that we don't try to
-- marshal nested maybes, as @Nothing@ can't be distinguished from @Just
-- Nothing@.
--
-- In particular, this uses the closed type family 'EnsureNotMaybe' to
-- throw a descriptive compile time when marshaling nested 'Maybe' types
-- is attempted. It also relies on a safe usage of
-- 'UndecidableInstances'.
module GHCJS.Marshal.Maybe
  ( EnsureNotMaybe
  ) where

import GHC.Prim
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Internal
import GHCJS.Marshal.Pure
import GHCJS.Types

-- | The result of 'EnsureNotMaybe' is a 'Constraint'. When its
-- parameter matches 'Maybe', it invokes an uninhabited type family in
-- order to cause a type error. Otherwise, it yields an empty
-- constraint, @()@.
type family EnsureNotMaybe a :: Constraint where
  EnsureNotMaybe (Maybe x) = Error "Can't correctly marshal nested Maybe types to / from JS."
  EnsureNotMaybe x = ()

type family Error (a :: k1) :: k2

instance (FromJSVal a, EnsureNotMaybe a) => FromJSVal (Maybe a) where
    fromJSValUnchecked x | isUndefined x || isNull x = return Nothing
                         | otherwise = fromJSVal x
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal x | isUndefined x || isNull x = return (Just Nothing)
                | otherwise = fmap (fmap Just) fromJSVal x
    {-# INLINE fromJSVal #-}

instance (ToJSVal a, EnsureNotMaybe a) => ToJSVal (Maybe a) where
    toJSVal Nothing  = return jsNull
    toJSVal (Just a) = toJSVal a
    {-# INLINE toJSVal #-}

instance (PFromJSVal a, EnsureNotMaybe a) => PFromJSVal (Maybe a) where
    pFromJSVal x | isUndefined x || isNull x = Nothing
    pFromJSVal x = Just (pFromJSVal x)
    {-# INLINE pFromJSVal #-}

instance (PToJSVal a, EnsureNotMaybe a) => PToJSVal (Maybe a) where
    pToJSVal Nothing  = jsNull
    pToJSVal (Just a) = pToJSVal a
    {-# INLINE pToJSVal #-}
