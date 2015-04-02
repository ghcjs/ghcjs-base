{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Cast ( Cast(..)
                       , cast
                       , unsafeCast
                       ) where

import GHCJS.Prim

cast :: forall a. Cast a => JSRef () -> Maybe a
cast x | js_checkCast x (instanceRef (undefined :: a)) = Just (unsafeWrap x)
       | otherwise                                     = Nothing
{-# INLINE cast #-}

unsafeCast :: Cast a => JSRef () -> a
unsafeCast x = unsafeWrap x
{-# INLINE unsafeCast #-}

class Cast a where
  unsafeWrap  :: JSRef () -> a
  instanceRef :: a -> JSRef ()

-- -----------------------------------------------------------------------------

foreign import javascript unsafe 
  "$1 instanceof $2" js_checkCast :: JSRef () -> JSRef () -> Bool
