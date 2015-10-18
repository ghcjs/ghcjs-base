{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MagicHash, UnboxedTuples, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JavaScript.TypedArray
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- Immutable operatons on JavaScript typed arrays
--
-----------------------------------------------------------------------------

#define DVIEW8(T), get/**/T, unsafeGet/**/T
#define DVIEW(T), get/**/T/**/LE, get/**/T/**/BE, unsafeGet/**/T/**/LE, unsafeGet/**/T/**/BE, unsafeGet/**/T, get/**/T

module JavaScript.TypedArray
    ( module JavaScript.TypedArray.Types
    , ArrayBufferData (..)
    , TypedArrayOperations (..)
    , arrayLength, arrayBuffer
    , dataView, dataView', unsafeDataView', dvByteLength, dvByteOffset, dvBuffer
    , ImmutableArrayBufferPrim (..)
    , MutableArrayBufferPrim (..)
    DVIEW(Int)
    DVIEW(Int32)
    DVIEW(Int16)
    DVIEW(Word)
    DVIEW(Word32)
    DVIEW(Word16)
    DVIEW(Float)
    DVIEW(Double)
    DVIEW8(Word8)
    DVIEW8(Int8)
    ) where


import GHC.Exts (State#, MutableByteArray#, ByteArray#)

import Data.Word
import Data.Int
import Data.Coerce
import Data.Primitive.ByteArray (ByteArray (..))
import Foreign.C.Types
import Unsafe.Coerce (unsafeCoerce)

import GHCJS.Internal.Types

import JavaScript.TypedArray.Internal
import JavaScript.TypedArray.Types



-- | Create a DataView for part of an ArrayBuffer.
--   Throws a `JSException' if the range specified by the
--   offset and length exceeds the size of the buffer
dataView' :: Int                 -- ^ start in bytes
          -> Maybe Int           -- ^ length in bytes, remainder of buffer if 'Nothing'
          -> SomeArrayBuffer (m :: MutabilityType s) -- ^ buffer to view
          -> SomeDataView (m :: MutabilityType s)
dataView' byteOffset mbyteLength (SomeArrayBuffer b) =
  case mbyteLength of
    Nothing -> js_dataView2 byteOffset b
    Just bl -> js_dataView byteOffset bl b
{-# INLINE dataView' #-}

-- | Create a DataView for part of an ArrayBuffer.
--   If the range specified by the offset and length exceeds the size
--   off the buffer, the resulting exception from the underlying call
--   kills the Haskell thread.
unsafeDataView' :: Int                 -- ^ start in bytes
                -> Maybe Int           -- ^ length in bytes, remainder of buffer if 'Nothing'
                -> SomeArrayBuffer (m :: MutabilityType s) -- ^ buffer to view
                -> SomeDataView (m :: MutabilityType s)
unsafeDataView' byteOffset mbyteLength (SomeArrayBuffer b) =
  case mbyteLength of
    Nothing -> js_dataView2 byteOffset b
    Just bl -> js_dataView byteOffset bl b
{-# INLINE unsafeDataView' #-}

-----------------------------------------------------------------------------
-- Helper instances
-----------------------------------------------------------------------------

instance Show (SomeTypedArray m t) where
    show = show . js_show



-----------------------------------------------------------------------------
-- | Convert data to primitive arrays
-----------------------------------------------------------------------------

class ImmutableArrayBufferPrim a where
    -- | Convert from primitive ByteArray without copying data
    fromByteArrayPrim :: ByteArray# -> a
    -- | Convert to primitive ByteArray without copying data
    toByteArrayPrim :: a -> ByteArray#
    -- | Convert from ByteArray without copying data
    fromByteArray :: ByteArray -> IO a
    fromByteArray (ByteArray ba) = pure (fromByteArrayPrim ba)
    {-# INLINE fromByteArray #-}
    -- | Convert to ByteArray without copying data
    toByteArray :: a -> IO ByteArray
    toByteArray b = pure $ ByteArray (toByteArrayPrim b)
    {-# INLINE toByteArray #-}

class MutableArrayBufferPrim a where
    -- | Convert from primitive MutableByteArray without copying data
    fromMutableByteArrayPrim :: MutableByteArray# s -> State# s -> (# State# s, a #)
    -- | Convert to primitive MutableByteArray without copying data
    toMutableByteArrayPrim :: a -> State# s -> (# State# s, MutableByteArray# s #)

-- SomeArrayBuffer instances

instance ImmutableArrayBufferPrim ArrayBuffer where
    {-# INLINE fromByteArrayPrim #-}
    fromByteArrayPrim = js_unwrapImmutableArrayBuffer
    {-# INLINE toByteArrayPrim #-}
    toByteArrayPrim = js_wrapImmutableArrayBuffer

instance MutableArrayBufferPrim (SomeArrayBuffer m) where
    {-# INLINE fromMutableByteArrayPrim #-}
    fromMutableByteArrayPrim = js_unwrapArrayBuffer
    {-# INLINE toMutableByteArrayPrim #-}
    toMutableByteArrayPrim = js_wrapArrayBuffer

-- SomeDataView instances

instance ImmutableArrayBufferPrim DataView where
    {-# INLINE fromByteArrayPrim #-}
    fromByteArrayPrim = js_unwrapImmutableDataView
    {-# INLINE toByteArrayPrim #-}
    toByteArrayPrim dv = js_wrapImmutableArrayBufferView (coerce dv)

instance MutableArrayBufferPrim (SomeDataView m) where
    {-# INLINE fromMutableByteArrayPrim #-}
    fromMutableByteArrayPrim = js_unwrapDataView
    {-# INLINE toMutableByteArrayPrim #-}
    toMutableByteArrayPrim dv = js_wrapArrayBufferView (coerce dv)

-- TypedArray instances

#define TYPEDARRAYPRIMCONVERT(T,JSType,JSSize)\
instance ImmutableArrayBufferPrim (TypedArray T) where{\
    {-# INLINE fromByteArrayPrim #-};\
    fromByteArrayPrim = js_unwrapImmutable/**/T/**/Array;\
    {-# INLINE toByteArrayPrim #-};\
    toByteArrayPrim arr = js_wrapImmutableArrayBufferView (coerce arr)};\
instance MutableArrayBufferPrim (SomeTypedArray m T) where{\
    {-# INLINE fromMutableByteArrayPrim #-};\
    fromMutableByteArrayPrim = js_unwrap/**/T/**/Array;\
    {-# INLINE toMutableByteArrayPrim #-};\
    toMutableByteArrayPrim arr = js_wrapArrayBufferView (coerce arr)}

TYPEDARRAYPRIMCONVERT(Int,Int32,4)
TYPEDARRAYPRIMCONVERT(Int32,Int32,4)
TYPEDARRAYPRIMCONVERT(Int16,Int16,2)
TYPEDARRAYPRIMCONVERT(Int8,Int8,1)
TYPEDARRAYPRIMCONVERT(Word,Uint32,4)
TYPEDARRAYPRIMCONVERT(Word32,Uint32,4)
TYPEDARRAYPRIMCONVERT(Word16,Uint16,2)
TYPEDARRAYPRIMCONVERT(Word8,Uint8,1)
TYPEDARRAYPRIMCONVERT(Word8Clamped,Uint8Clamped,1)
TYPEDARRAYPRIMCONVERT(Float,Float32,4)
TYPEDARRAYPRIMCONVERT(Double,Float64,8)
TYPEDARRAYPRIMCONVERT(CChar,Int8,1)
TYPEDARRAYPRIMCONVERT(CSChar,Int8,1)
TYPEDARRAYPRIMCONVERT(CUChar,Uint8,1)
TYPEDARRAYPRIMCONVERT(CShort,Int16,2)
TYPEDARRAYPRIMCONVERT(CUShort,Uint16,2)
TYPEDARRAYPRIMCONVERT(CInt,Int32,4)
TYPEDARRAYPRIMCONVERT(CUInt,Uint32,4)
TYPEDARRAYPRIMCONVERT(CLong,Int32,4)
TYPEDARRAYPRIMCONVERT(CULong,Uint32,4)
TYPEDARRAYPRIMCONVERT(CFloat,Float32,4)
TYPEDARRAYPRIMCONVERT(CDouble,Float64,8)

-----------------------------------------------------------------------------
-- | Common functions on buffers and views
-----------------------------------------------------------------------------

class ArrayBufferData a where
    -- | Length of buffer or its view in bytes
    byteLength :: a -> Int
    -- | Slice array (elements) or buffer (bytes).
    --   See documentation on TypedArray.prototype.slice() and ArrayBuffer.prototype.slice()
    sliceImmutable :: Int -> Maybe Int -> a -> a

instance ArrayBufferData (SomeArrayBuffer m) where
    {-# INLINE byteLength #-}
    byteLength = js_byteLength . coerce
    {-# INLINE sliceImmutable #-}
    sliceImmutable i0 Nothing arr = coerce $ js_slice1_imm i0 (coerce arr)
    sliceImmutable i0 (Just i1) arr = coerce $ js_slice_imm i0 i1 (coerce arr)

instance ArrayBufferData (SomeTypedArray m t) where
    {-# INLINE byteLength #-}
    byteLength = js_byteLength . coerce
    {-# INLINE sliceImmutable #-}
    sliceImmutable i0 Nothing arr = coerce $ js_slice1_imm i0 (coerce arr)
    sliceImmutable i0 (Just i1) arr = coerce $ js_slice_imm i0 i1 (coerce arr)



-----------------------------------------------------------------------------
-- | Typed array immutable functions
-----------------------------------------------------------------------------

class TypedArrayOperations a where
    -- | Init a new typed array filled with zeroes
    typedArray :: Int -> TypedArray a
    -- | Fill a new typed array with a given value
    fillNewTypedArray :: Int -> a -> TypedArray a
    -- | Create a new typed array from list
    fromList :: [a] -> TypedArray a
    -- | Create a new typed array from elements of another typed array
    fromArray :: TypedArray b -> TypedArray a
    -- | Create a typed array view on a given array buffer (do not copy data)
    arrayView :: SomeArrayBuffer (m :: MutabilityType s) -> SomeTypedArray (m :: MutabilityType s) a
    -- | Index typed array
    (!) :: TypedArray a -> Int -> a
    -- | Size of an array element, in bytes
    elemSize :: SomeTypedArray (m :: MutabilityType s) a -> Int
    -- | First occurence of a given element in the array, starting from specified index
    indexOf :: Int -> a -> TypedArray a -> Int
    -- | Last occurence of a given element in the array, search backwards starting from specified index
    lastIndexOf :: Int -> a -> TypedArray a -> Int


#define TYPEDARRAY(T,JSType,JSSize)\
instance TypedArrayOperations T where{\
    {-# INLINE typedArray #-};\
    typedArray = js_create/**/T/**/Array;\
    {-# INLINE fillNewTypedArray #-};\
    fillNewTypedArray = js_fillNew/**/T/**/Array;\
    {-# INLINE fromList #-};\
    fromList = js_fromList/**/T/**/Array . unsafeCoerce . seqList;\
    {-# INLINE fromArray #-};\
    fromArray = js_fromArray/**/T/**/Array;\
    {-# INLINE arrayView #-};\
    arrayView = js_view/**/T/**/Array;\
    {-# INLINE (!) #-};\
    (!) = js_index/**/T/**/Array;\
    {-# INLINE elemSize #-};\
    elemSize _ = JSSize;\
    {-# INLINE indexOf #-};\
    indexOf = js_indexOf/**/T/**/Array;\
    {-# INLINE lastIndexOf #-};\
    lastIndexOf = js_lastIndexOf/**/T/**/Array}


TYPEDARRAY(Int,Int32,4)
TYPEDARRAY(Int32,Int32,4)
TYPEDARRAY(Int16,Int16,2)
TYPEDARRAY(Int8,Int8,1)
TYPEDARRAY(Word,Uint32,4)
TYPEDARRAY(Word32,Uint32,4)
TYPEDARRAY(Word16,Uint16,2)
TYPEDARRAY(Word8,Uint8,1)
TYPEDARRAY(Word8Clamped,Uint8Clamped,1)
TYPEDARRAY(Float,Float32,4)
TYPEDARRAY(Double,Float64,8)
TYPEDARRAY(CChar,Int8,1)
TYPEDARRAY(CSChar,Int8,1)
TYPEDARRAY(CUChar,Uint8,1)
TYPEDARRAY(CShort,Int16,2)
TYPEDARRAY(CUShort,Uint16,2)
TYPEDARRAY(CInt,Int32,4)
TYPEDARRAY(CUInt,Uint32,4)
TYPEDARRAY(CLong,Int32,4)
TYPEDARRAY(CULong,Uint32,4)
TYPEDARRAY(CFloat,Float32,4)
TYPEDARRAY(CDouble,Float64,8)


-----------------------------------------------------------------------------
-- DataView immutable functions
-----------------------------------------------------------------------------

#define DATAVIEW8(T,JSType,JSSize)\
get/**/T, unsafeGet/**/T\
  :: Int -> DataView -> T;\
get/**/T       = js_i_safeGet/**/T;\
unsafeGet/**/T = js_i_unsafeGet/**/T;\
{-# INLINE get/**/T #-};\
{-# INLINE unsafeGet/**/T #-};

#define DATAVIEW(T,JSType,JSSize)\
get/**/T/**/LE, get/**/T/**/BE, unsafeGet/**/T/**/LE, unsafeGet/**/T/**/BE, get/**/T, unsafeGet/**/T\
  :: Int -> DataView -> T;\
get/**/T/**/LE       = js_i_safeGet/**/T/**/LE;\
get/**/T/**/BE       = js_i_safeGet/**/T/**/BE;\
unsafeGet/**/T/**/LE = js_i_unsafeGet/**/T/**/LE;\
unsafeGet/**/T/**/BE = js_i_unsafeGet/**/T/**/BE;\
{- | Shortcut for little-endian -};\
get/**/T       = get/**/T/**/LE;\
{- | Shortcut for little-endian -};\
unsafeGet/**/T = unsafeGet/**/T/**/LE;\
{-# INLINE get/**/T/**/LE #-};\
{-# INLINE get/**/T/**/BE #-};\
{-# INLINE get/**/T #-};\
{-# INLINE unsafeGet/**/T/**/LE #-};\
{-# INLINE unsafeGet/**/T/**/BE #-};\
{-# INLINE unsafeGet/**/T #-};

DATAVIEW(Int,Int32,4)
DATAVIEW(Int32,Int32,4)
DATAVIEW(Int16,Int16,2)
DATAVIEW(Word,Uint32,4)
DATAVIEW(Word32,Uint32,4)
DATAVIEW(Word16,Uint16,2)
DATAVIEW(Float,Float32,4)
DATAVIEW(Double,Float64,8)

DATAVIEW8(Word8,Uint8,1)
DATAVIEW8(Int8,Int8,1)

