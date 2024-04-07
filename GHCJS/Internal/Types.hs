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

import GHC.JS.Prim (JSVal)
import GHC.JS.Foreign.Callback (Callback)

instance NFData JSVal where
  rnf x = x `seq` ()

class IsJSVal a where
  jsval_ :: a -> JSVal

  default jsval_ :: Coercible a JSVal => a -> JSVal
  jsval_ = coerce
  {-# INLINE jsval_ #-}

instance IsJSVal (Callback a) where
  jsval_ = unsafeCoerce

jsval :: IsJSVal a => a -> JSVal
jsval = jsval_
{-# INLINE jsval #-}

data MutabilityType s = Mutable_ s
                      | Immutable_ s
                      | STMutable s

type Mutable   = Mutable_ ()
type Immutable = Immutable_ ()

data IsItMutable = IsImmutable
                 | IsMutable

type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability Immutable     = IsImmutable
  Mutability Mutable       = IsMutable
  Mutability (STMutable s) = IsMutable


