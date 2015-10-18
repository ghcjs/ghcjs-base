{-# LANGUAGE DeriveDataTypeable,GeneralizedNewtypeDeriving, TypeFamilies, CPP #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JavaScript.TypedArray.Types
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module JavaScript.TypedArray.Types where


import Data.Typeable (Typeable)
import Data.Word (Word8)
import Data.Ix (Ix)
import Data.Data (Data)
import Data.Bits (Bits, FiniteBits)
import Foreign.Storable (Storable)

import GHCJS.Internal.Types
import GHCJS.Marshal.Pure
import GHCJS.Types

-- | Stub for Uint8ClampedArray in JS
newtype Word8Clamped = Clamped Word8 deriving
    (Ord,Num,Eq,Bounded,Enum,Integral,Data,Real,Show,Ix,FiniteBits,Bits,Storable)


type TypedArray a = SomeTypedArray 'Immutable a
type STTypedArray s a = SomeTypedArray ('STMutable s) a
type IOTypedArray a = SomeTypedArray 'Mutable a

-- | Any typed array, mutable or immutable
newtype SomeTypedArray (m :: MutabilityType s) (a :: *) = SomeTypedArray JSVal deriving Typeable
instance IsJSVal (SomeTypedArray m a)

instance PToJSVal (SomeTypedArray m a) where
  pToJSVal (SomeTypedArray v) = v
instance PFromJSVal (SomeTypedArray m a) where
  pFromJSVal = SomeTypedArray

-- | ArrayBuffer, mutable or immutable
newtype SomeArrayBuffer (a :: MutabilityType s) = SomeArrayBuffer JSVal deriving Typeable
instance IsJSVal (SomeArrayBuffer m)

type ArrayBuffer      = SomeArrayBuffer 'Immutable
type IOArrayBuffer    = SomeArrayBuffer 'Mutable
type STArrayBuffer s  = SomeArrayBuffer ('STMutable s)

instance PToJSVal (SomeArrayBuffer m) where
    pToJSVal (SomeArrayBuffer b) = b
instance PFromJSVal (SomeArrayBuffer m) where
    pFromJSVal = SomeArrayBuffer

-- | Data view on ArrayBuffer, mutable or immutable
newtype SomeDataView (a :: MutabilityType s) = SomeDataView JSVal deriving Typeable
instance IsJSVal (SomeDataView m)


type DataView     = SomeDataView 'Immutable
type IODataView   = SomeDataView 'Mutable
type STDataView s = SomeDataView ('STMutable s)

instance PToJSVal (SomeDataView m) where
    pToJSVal (SomeDataView b) = b
instance PFromJSVal (SomeDataView m) where
    pFromJSVal = SomeDataView


