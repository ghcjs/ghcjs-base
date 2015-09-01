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

import GHCJS.Prim (JSRef)

instance NFData JSRef where
  rnf x = x `seq` ()

class IsJSRef a where
  jsref_ :: a -> JSRef

  default jsref_ :: Coercible a JSRef => a -> JSRef
  jsref_ = coerce
  {-# INLINE jsref_ #-}

jsref :: IsJSRef a => a -> JSRef
jsref = jsref_
{-# INLINE jsref #-}

data MutabilityType s = Mutable
                      | Immutable
                      | STMutable s

data IsItMutable = IsImmutable
                 | IsMutable

type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability Immutable     = IsImmutable
  Mutability Mutable       = IsMutable
  Mutability (STMutable s) = IsMutable


