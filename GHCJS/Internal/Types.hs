{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module GHCJS.Internal.Types where

import Data.Coerce
import Unsafe.Coerce

import Control.DeepSeq

import GHCJS.Prim (JSVal)

instance NFData JSVal where
  rnf x = x `seq` ()

instance IsJSVal JSVal

-- | Instances of this class should be newtype wrappers around 'JSVal`. It
-- should never be necessary to provide definitions for the methods, as
-- they have defaults in terms of 'Coercible'. This is why the methods
-- aren't exported by "GHCJS.Types".
class IsJSVal a where
  jsval_ :: a -> JSVal
  default jsval_ :: Coercible a JSVal => a -> JSVal
  jsval_ = coerce
  {-# INLINE jsval_ #-}

  uncheckedWrapJSVal_ :: JSVal -> a
  default uncheckedWrapJSVal_ :: Coercible a JSVal => JSVal -> a
  uncheckedWrapJSVal_ = coerce
  {-# INLINE uncheckedWrapJSVal_ #-}

-- | This gets the 'JSVal' stored within a newtype wrapper.
jsval :: IsJSVal a => a -> JSVal
jsval = jsval_
{-# INLINE jsval #-}

-- | This is an unchecked downcast from 'JSVal' to some newtype wrapper.
-- Use with care, because this is an unchecked downcast. It should only
-- be used when you know that the 'JSVal' is a valid inhabitant of the
-- newtype.
uncheckedWrapJSVal :: IsJSVal a => JSVal -> a
uncheckedWrapJSVal = uncheckedWrapJSVal_
{-# INLINE uncheckedWrapJSVal #-}

data MutabilityType s = Mutable
                      | Immutable
                      | STMutable s

data IsItMutable = IsImmutable
                 | IsMutable

type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability Immutable     = IsImmutable
  Mutability Mutable       = IsMutable
  Mutability (STMutable s) = IsMutable
