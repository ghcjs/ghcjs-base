{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JavaScript.TypedArray.IO
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- Mutable operatons on JavaScript typed arrays in IO monad
--
-----------------------------------------------------------------------------

module JavaScript.TypedArray.IO where

import GHC.Exts (ByteArray#, MutableByteArray#)
import qualified GHC.Types as Exts

import Control.Monad.Primitive (PrimState (..))

import Data.Coerce (coerce)
import Data.Word
import Data.Int
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray (..))
import Foreign.C.Types
import Unsafe.Coerce (unsafeCoerce)


import GHCJS.Internal.Types

import JavaScript.TypedArray
import JavaScript.TypedArray.Types
import JavaScript.TypedArray.Internal

-----------------------------------------------------------------------------
-- | mutable typed arrays
-----------------------------------------------------------------------------

class IOTypedArrayOperations a where
    -- | Init a new typed array filled with zeroes
    newIOTypedArray :: Int -> IO (IOTypedArray a)
    -- | Fill a new typed array with a given value
    fillNewIOTypedArray :: Int -> a -> IO (IOTypedArray a)
    -- | Create a new typed array from list
    newFromList :: [a] -> IO (IOTypedArray a)
    -- | Create a new typed array from elements of another typed array
    newFromArray :: SomeTypedArray (m :: MutabilityType sk) b -> IO (IOTypedArray a)
    -- | Get value from array at specified index
    index :: Int -> IOTypedArray a -> IO a
    -- | Set value into array at specified index
    setIndex ::Int -> a -> IOTypedArray a -> IO ()
    -- | Set list into array with specified offset
    setList :: Int -> [a] -> IOTypedArray a -> IO ()
    -- | Set array elements into array with specified offset
    setArray :: Int -> SomeTypedArray (m :: MutabilityType sk) b -> IOTypedArray a -> IO ()



#define TYPEDARRAY(T,JSType,JSSize)\
instance IOTypedArrayOperations T where{\
    {-# INLINE newIOTypedArray #-};\
    newIOTypedArray n = Exts.IO (js_createM/**/T/**/Array n);\
    {-# INLINE fillNewIOTypedArray #-};\
    fillNewIOTypedArray n v = Exts.IO (js_fillNewM/**/T/**/Array n v);\
    {-# INLINE newFromList #-};\
    newFromList xs = Exts.IO (js_fromListM/**/T/**/Array . unsafeCoerce . seqList $ xs);\
    {-# INLINE newFromArray #-};\
    newFromArray arr = Exts.IO (js_fromArrayM/**/T/**/Array arr);\
    {-# INLINE index #-};\
    index i arr = Exts.IO (js_getIndex/**/T/**/Array i arr);\
    {-# INLINE setIndex #-};\
    setIndex i v arr = Exts.IO (js_setIndex/**/T/**/Array i v arr);\
    {-# INLINE setList #-};\
    setList offset xs arr = Exts.IO (js_setList/**/T/**/Array offset (unsafeCoerce $ seqList xs) arr);\
    {-# INLINE setArray #-};\
    setArray offset ar0 arr = Exts.IO (js_setArray/**/T/**/Array offset ar0 arr)}


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
-- | mutable anything
-----------------------------------------------------------------------------



class IOArrayBufferData mutable any | any -> mutable where
    -- | Slice array (elements) or buffer (bytes).
    --   See documentation on TypedArray.prototype.slice() and ArrayBuffer.prototype.slice()
    slice :: Int -> Maybe Int -> any -> IO mutable


class ( MutableArrayBufferPrim mutable
      ) => IOArrayBufferConversions immutable mutable
        | immutable -> mutable
        , mutable -> immutable where
    -- | Create an immutable data by copying a mutable data
    freeze :: mutable -> IO immutable
    -- | Create an immutable data from a mutable data without
    --   copying. The result shares the buffer with the argument, do not modify
    --   the data in the original buffer after freezing
    unsafeFreeze :: mutable -> IO immutable
    -- | Create a mutable data by copying an immutable data
    thaw :: immutable -> IO mutable
    -- | Create a mutable data from an immutable data without
    --   copying. The result shares the buffer with the argument.
    unsafeThaw :: immutable -> IO mutable
    -- | Convert from MutableByteArray without copying data
    fromMutableByteArray :: MutableByteArray (PrimState IO) -> IO mutable
    fromMutableByteArray (MutableByteArray ba) = Exts.IO (fromMutableByteArrayPrim ba)
    {-# INLINE fromMutableByteArray #-}
    -- | Convert to MutableByteArray without copying data
    toMutableByteArray :: mutable -> IO (MutableByteArray (PrimState IO))
    toMutableByteArray b = Exts.IO $ \s ->
        case toMutableByteArrayPrim b s of (# s1, ba #) -> (# s1, MutableByteArray ba #)
    {-# INLINE toMutableByteArray #-}


instance IOArrayBufferData IOArrayBuffer (SomeArrayBuffer m) where
    {-# INLINE slice #-}
    slice i0 Nothing (SomeArrayBuffer b) = fmap SomeArrayBuffer . Exts.IO $ js_slice1 i0 b
    slice i0 (Just i1) (SomeArrayBuffer b) = fmap SomeArrayBuffer . Exts.IO $ js_slice i0 i1 b

instance IOArrayBufferConversions ArrayBuffer IOArrayBuffer where
    {-# INLINE freeze #-}
    freeze (SomeArrayBuffer b) = fmap SomeArrayBuffer (Exts.IO (js_slice1 0 b))
    {-# INLINE unsafeFreeze #-}
    unsafeFreeze (SomeArrayBuffer b) = pure (SomeArrayBuffer b)
    {-# INLINE thaw #-}
    thaw (SomeArrayBuffer b) = fmap SomeArrayBuffer (Exts.IO (js_slice1 0 b))
    {-# INLINE unsafeThaw #-}
    unsafeThaw (SomeArrayBuffer b) = pure (SomeArrayBuffer b)


instance IOArrayBufferData (IOTypedArray t) (SomeTypedArray m t) where
    {-# INLINE slice #-}
    slice i0 Nothing (SomeTypedArray b) = fmap SomeTypedArray . Exts.IO $ js_slice1 i0 b
    slice i0 (Just i1) (SomeTypedArray b) = fmap SomeTypedArray . Exts.IO $ js_slice i0 i1 b

instance ( MutableArrayBufferPrim (IOTypedArray t)
         ) => IOArrayBufferConversions (TypedArray t) (IOTypedArray t) where
    {-# INLINE freeze #-}
    freeze (SomeTypedArray b) = fmap SomeTypedArray (Exts.IO (js_slice1 0 b))
    {-# INLINE unsafeFreeze #-}
    unsafeFreeze (SomeTypedArray b) = pure (SomeTypedArray b)
    {-# INLINE thaw #-}
    thaw (SomeTypedArray b) = fmap SomeTypedArray (Exts.IO (js_slice1 0 b))
    {-# INLINE unsafeThaw #-}
    unsafeThaw (SomeTypedArray b) = pure (SomeTypedArray b)

instance IOArrayBufferConversions DataView IODataView where
    {-# INLINE freeze #-}
    freeze dv = Exts.IO (js_cloneDataView dv)
    {-# INLINE unsafeFreeze #-}
    unsafeFreeze (SomeDataView b) = pure (SomeDataView b)
    {-# INLINE thaw #-}
    thaw dv = Exts.IO (js_cloneDataView dv)
    {-# INLINE unsafeThaw #-}
    unsafeThaw (SomeDataView b) = pure (SomeDataView b)

#define DATAVIEW8(T,JSType,JSSize)\
write/**/T, unsafeWrite/**/T\
  :: Int -> T -> IODataView -> IO ();\
write/**/T       idx x dv = Exts.IO (js_safeSet/**/T   idx x dv);\
unsafeWrite/**/T idx x dv = Exts.IO (js_unsafeSet/**/T idx x dv);\
{-# INLINE write/**/T #-};\
{-# INLINE unsafeWrite/**/T #-};\
read/**/T, unsafeRead/**/T\
  :: Int -> IODataView -> IO T;\
read/**/T       idx dv = Exts.IO (js_m_safeGet/**/T   idx dv);\
unsafeRead/**/T idx dv = Exts.IO (js_m_unsafeGet/**/T idx dv);\
{-# INLINE read/**/T #-};\
{-# INLINE unsafeRead/**/T #-};

#define DATAVIEW(T,JSType,JSSize)\
write/**/T/**/LE, write/**/T/**/BE, unsafeWrite/**/T/**/LE, unsafeWrite/**/T/**/BE, write/**/T, unsafeWrite/**/T\
  :: Int -> T -> IODataView -> IO ();\
write/**/T/**/LE       idx x dv = Exts.IO (js_safeSet/**/T/**/LE   idx x dv);\
write/**/T/**/BE       idx x dv = Exts.IO (js_safeSet/**/T/**/BE   idx x dv);\
unsafeWrite/**/T/**/LE idx x dv = Exts.IO (js_unsafeSet/**/T/**/LE idx x dv);\
unsafeWrite/**/T/**/BE idx x dv = Exts.IO (js_unsafeSet/**/T/**/BE idx x dv);\
{- | Shortcut for little-endian -};\
write/**/T       = write/**/T/**/LE;\
{- | Shortcut for little-endian -};\
unsafeWrite/**/T = unsafeWrite/**/T/**/LE;\
{-# INLINE write/**/T/**/LE #-};\
{-# INLINE write/**/T/**/BE #-};\
{-# INLINE unsafeWrite/**/T/**/LE #-};\
{-# INLINE unsafeWrite/**/T/**/BE #-};\
{-# INLINE write/**/T #-};\
{-# INLINE unsafeWrite/**/T #-};\
read/**/T/**/LE, read/**/T/**/BE, unsafeRead/**/T/**/LE, unsafeRead/**/T/**/BE, read/**/T, unsafeRead/**/T\
  :: Int -> IODataView -> IO T;\
read/**/T/**/LE       idx dv = Exts.IO (js_m_safeGet/**/T/**/LE   idx dv);\
read/**/T/**/BE       idx dv = Exts.IO (js_m_safeGet/**/T/**/BE   idx dv);\
unsafeRead/**/T/**/LE idx dv = Exts.IO (js_m_unsafeGet/**/T/**/LE idx dv);\
unsafeRead/**/T/**/BE idx dv = Exts.IO (js_m_unsafeGet/**/T/**/BE idx dv);\
{- | Shortcut for little-endian -};\
read/**/T       = read/**/T/**/LE;\
{- | Shortcut for little-endian -};\
unsafeRead/**/T = unsafeRead/**/T/**/LE;\
{-# INLINE read/**/T/**/LE #-};\
{-# INLINE read/**/T/**/BE #-};\
{-# INLINE unsafeRead/**/T/**/LE #-};\
{-# INLINE unsafeRead/**/T/**/BE #-};\
{-# INLINE read/**/T #-};\
{-# INLINE unsafeRead/**/T #-};

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

-----------------------------------------------------------------------------
-- Misc
-----------------------------------------------------------------------------

-- | Create new array buffer
newIOArrayBuffer :: Int -> IO IOArrayBuffer
newIOArrayBuffer size = Exts.IO (js_createArrayBuffer size)

