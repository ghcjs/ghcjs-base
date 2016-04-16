{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnboxedTuples, JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# LANGUAGE PolyKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypedArray.Internal
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- JS imports used by typed arrays
--
-----------------------------------------------------------------------------

module JavaScript.TypedArray.Internal where

import GHC.Exts (State#, MutableByteArray#, ByteArray#)
import qualified GHC.Exts as Exts

import Data.Coerce (coerce)
import Data.Word
import Data.Int
import Foreign.C.Types

import GHCJS.Types

import JavaScript.TypedArray.Types


-----------------------------------------------------------------------------
-- Some simple functions -- the same for many types
-----------------------------------------------------------------------------


{-# INLINE arrayLength #-}
-- | Number of elements in the array
foreign import javascript unsafe "$1.length"
    arrayLength :: SomeTypedArray m a -> Int

{-# INLINE arrayBuffer #-}
-- | Get underlying ArrayBuffer
foreign import javascript unsafe "$1.buffer"
    arrayBuffer :: SomeTypedArray m a -> SomeArrayBuffer m

{-# INLINE dataView #-}
-- | Create a DataView for the whole ArrayBuffer
foreign import javascript unsafe "new DataView($1)"
    dataView :: SomeArrayBuffer m -> SomeDataView m

{-# INLINE dvByteLength #-}
-- | Size of DataView in bytes
foreign import javascript unsafe "$1.byteLength"
    dvByteLength :: SomeDataView m -> Int
{-# INLINE dvByteOffset #-}
-- | Offset of DataView within a buffer in bytes
foreign import javascript unsafe "$1.byteOffset"
    dvByteOffset :: SomeDataView m -> Int
{-# INLINE dvBuffer #-}
-- | Underlying ArrayBuffer of DataView
foreign import javascript unsafe "$1.buffer"
    dvBuffer :: SomeDataView m -> SomeArrayBuffer m

-----------------------------------------------------------------------------
-- Some not exposed js imports
-----------------------------------------------------------------------------

{-# INLINE js_byteLength #-}
foreign import javascript unsafe "$1.byteLength"
    js_byteLength :: JSVal -> Int

{-# INLINE js_createArrayBuffer #-}
foreign import javascript unsafe "new ArrayBuffer($1)"
    js_createArrayBuffer :: Int -> State# s -> (# State# s, SomeArrayBuffer m #)

{-# INLINE js_show #-}
foreign import javascript unsafe "$r = '[' + $1.join(', ') + ']'"
    js_show :: SomeTypedArray m t -> JSString



-- slice mutable any

{-# INLINE js_slice1 #-}
foreign import javascript unsafe
  "$2.slice($1)" js_slice1 :: Int -> JSVal -> State# s -> (# State# s, JSVal #)

{-# INLINE js_slice #-}
foreign import javascript unsafe
  "$3.slice($1,$2)" js_slice :: Int -> Int -> JSVal -> State# s -> (# State# s, JSVal #)


-- slice immutable any

{-# INLINE js_slice1_imm #-}
foreign import javascript unsafe
  "$2.slice($1)" js_slice1_imm :: Int -> JSVal -> JSVal

{-# INLINE js_slice_imm #-}
foreign import javascript unsafe
  "$3.slice($1,$2)" js_slice_imm :: Int -> Int -> JSVal -> JSVal

-- Creating data views


{-# INLINE js_dataView2 #-}
foreign import javascript safe "new DataView($2,$1)"
    js_dataView2 :: Int -> JSVal -> SomeDataView m
{-# INLINE js_unsafeDataView2 #-}
foreign import javascript unsafe "new DataView($2,$1)"
    js_unsafeDataView2 :: Int -> JSVal-> SomeDataView m
{-# INLINE js_dataView #-}
foreign import javascript safe "new DataView($3,$1,$2)"
    js_dataView :: Int -> Int -> JSVal -> SomeDataView m
{-# INLINE js_unsafeDataView #-}
foreign import javascript unsafe "new DataView($3,$1,$2)"
    js_unsafeDataView :: Int -> Int -> JSVal -> JSVal

{-# INLINE js_cloneDataView #-}
foreign import javascript unsafe "new DataView($1.buffer.slice($1.byteOffset, $1.byteLength))"
    js_cloneDataView :: SomeDataView m0 -> State# s -> (# State# s, SomeDataView m #)

-----------------------------------------------------------------------------
-- All mutable data functions
-----------------------------------------------------------------------------

#define CREATEFUNCTIONS(T , JSName, JSArray, JSSize) \
foreign import javascript unsafe "new JSArray($1)" js_createM/**/T/**/Array :: Int -> State# s -> (# State# s, SomeTypedArray m T #); {-# INLINE js_createM/**/T/**/Array #-};\
foreign import javascript unsafe "new JSArray($1).fill($2)" js_fillNewM/**/T/**/Array :: Int -> T -> State# s -> (# State# s, SomeTypedArray m T #); {-# INLINE js_fillNewM/**/T/**/Array #-};\
foreign import javascript unsafe "var arr = h$fromListPrim($1); $r = new JSArray(arr.length); $r.set(arr);" js_fromListM/**/T/**/Array :: Exts.Any -> State# s -> (# State# s, SomeTypedArray m T #); {-# INLINE js_fromListM/**/T/**/Array #-};\
foreign import javascript unsafe "$r = new JSArray($1.length); $r.set($1);" js_fromArrayM/**/T/**/Array :: SomeTypedArray m0 t -> State# s -> (# State# s, SomeTypedArray m T #); {-# INLINE js_fromArrayM/**/T/**/Array #-};\
foreign import javascript unsafe "$3[$1] = $2" js_setIndex/**/T/**/Array :: Int -> T -> SomeTypedArray m T -> State# s -> (# State# s, () #); {-# INLINE js_setIndex/**/T/**/Array #-};\
foreign import javascript unsafe "$3.set(h$fromListPrim($2), $1)" js_setList/**/T/**/Array :: Int -> Exts.Any -> SomeTypedArray m T -> State# s -> (# State# s, () #); {-# INLINE js_setList/**/T/**/Array #-};\
foreign import javascript unsafe "$3.set($2, $1)" js_setArray/**/T/**/Array :: Int -> SomeTypedArray m0 t -> SomeTypedArray m T -> State# s -> (# State# s, () #); {-# INLINE js_setArray/**/T/**/Array #-};

CREATEFUNCTIONS(Int,Int32,Int32Array,4)
CREATEFUNCTIONS(Int32,Int32,Int32Array,4)
CREATEFUNCTIONS(Int16,Int16,Int16Array,2)
CREATEFUNCTIONS(Int8,Int8,Int8Array,1)
CREATEFUNCTIONS(Word,Uint32,Uint32Array,4)
CREATEFUNCTIONS(Word32,Uint32,Uint32Array,4)
CREATEFUNCTIONS(Word16,Uint16,Uint16Array,2)
CREATEFUNCTIONS(Word8,Uint8,Uint8Array,1)
CREATEFUNCTIONS(Float,Float32,Float32Array,4)
CREATEFUNCTIONS(Double,Float64,Float64Array,8)
CREATEFUNCTIONS(CChar,Int8,Int8Array,1)
CREATEFUNCTIONS(CSChar,Int8,Int8Array,1)
CREATEFUNCTIONS(CUChar,Uint8,Uint8Array,1)
CREATEFUNCTIONS(CShort,Int16,Int16Array,2)
CREATEFUNCTIONS(CUShort,Uint16,Uint16Array,2)
CREATEFUNCTIONS(CInt,Int32,Int32Array,4)
CREATEFUNCTIONS(CUInt,Uint32,Uint32Array,4)
CREATEFUNCTIONS(CLong,Int32,Int32Array,4)
CREATEFUNCTIONS(CULong,Uint32,Uint32Array,4)
CREATEFUNCTIONS(CFloat,Float32,Float32Array,4)
CREATEFUNCTIONS(CDouble,Float64,Float64Array,8)
CREATEFUNCTIONS(Word8Clamped,Uint8Clamped,Uint8ClampedArray,1)

-- js_getIndexXXXArray operation for newtypes

#define CREATEGETFUNCTION(T , JSName, JSArray, JSSize) \
foreign import javascript unsafe "$2[$1]" js_getIndex/**/T/**/Array :: Int -> SomeTypedArray m T -> State# s -> (# State# s, T #); {-# INLINE js_getIndex/**/T/**/Array #-};

CREATEGETFUNCTION(Int,Int32,Int32Array,4)
CREATEGETFUNCTION(Int32,Int32,Int32Array,4)
CREATEGETFUNCTION(Int16,Int16,Int16Array,2)
CREATEGETFUNCTION(Int8,Int8,Int8Array,1)
CREATEGETFUNCTION(Word,Uint32,Uint32Array,4)
CREATEGETFUNCTION(Word32,Uint32,Uint32Array,4)
CREATEGETFUNCTION(Word16,Uint16,Uint16Array,2)
CREATEGETFUNCTION(Word8,Uint8,Uint8Array,1)
CREATEGETFUNCTION(Float,Float32,Float32Array,4)
CREATEGETFUNCTION(Double,Float64,Float64Array,8)

#define CREATEGETFUNCTIONNT(T , T2, JSName, JSArray, JSSize) \
{-# INLINE js_getIndex/**/T/**/Array #-};\
js_getIndex/**/T/**/Array :: Int -> SomeTypedArray m T -> State# s -> (# State# s, T #);\
js_getIndex/**/T/**/Array i arr s = case js_getIndex/**/T2/**/Array i (coerce arr) s of { (# s1, v #) -> (# s1, coerce v #) }

CREATEGETFUNCTIONNT(CChar,Int8,Int8,Int8Array,1)
CREATEGETFUNCTIONNT(CSChar,Int8,Int8,Int8Array,1)
CREATEGETFUNCTIONNT(CUChar,Word8,Uint8,Uint8Array,1)
CREATEGETFUNCTIONNT(CShort,Int16,Int16,Int16Array,2)
CREATEGETFUNCTIONNT(CUShort,Word16,Uint16,Uint16Array,2)
CREATEGETFUNCTIONNT(CInt,Int32,Int32,Int32Array,4)
CREATEGETFUNCTIONNT(CUInt,Word32,Uint32,Uint32Array,4)
CREATEGETFUNCTIONNT(CLong,Int32,Int32,Int32Array,4)
CREATEGETFUNCTIONNT(CULong,Word32,Uint32,Uint32Array,4)
CREATEGETFUNCTIONNT(CFloat,Float,Float32,Float32Array,4)
CREATEGETFUNCTIONNT(CDouble,Double,Float64,Float64Array,8)
CREATEGETFUNCTIONNT(Word8Clamped,Word8,Uint8Clamped,Uint8ClampedArray,1)


-----------------------------------------------------------------------------
-- Conversions between types
-----------------------------------------------------------------------------

{-# INLINE js_wrapImmutableArrayBuffer #-};
foreign import javascript unsafe
    "h$wrapBuffer" js_wrapImmutableArrayBuffer :: SomeArrayBuffer m -> ByteArray#
{-# INLINE js_unwrapImmutableArrayBuffer #-};
foreign import javascript unsafe
    "h$wrapBuffer" js_unwrapImmutableArrayBuffer :: ByteArray# -> SomeArrayBuffer m
{-# INLINE js_wrapArrayBuffer #-};
foreign import javascript unsafe
    "h$wrapBuffer" js_wrapArrayBuffer :: SomeArrayBuffer any -> State# s -> (# State# s, MutableByteArray# s #)
{-# INLINE js_unwrapArrayBuffer #-};
foreign import javascript unsafe
    "h$wrapBuffer" js_unwrapArrayBuffer :: MutableByteArray# s -> State# s -> (# State# s, SomeArrayBuffer any #)

{-# INLINE js_wrapImmutableArrayBufferView #-};
foreign import javascript unsafe
    "h$wrapBuffer($1.buffer)" js_wrapImmutableArrayBufferView :: JSVal -> ByteArray#
{-# INLINE js_wrapArrayBufferView #-};
foreign import javascript unsafe
    "h$wrapBuffer($1.buffer)" js_wrapArrayBufferView :: JSVal -> State# s -> (# State# s, MutableByteArray# s #)

{-# INLINE js_unwrapImmutableDataView #-};
foreign import javascript unsafe
    "$1.dv" js_unwrapImmutableDataView :: ByteArray# -> SomeDataView m
{-# INLINE js_unwrapDataView #-};
foreign import javascript unsafe
    "$1.dv" js_unwrapDataView :: MutableByteArray# s -> State# s -> (# State# s, SomeDataView m #)

#define CREATECONVERTERS(T, JSPType, JSName, JSArray, JSSize) \
foreign import javascript unsafe "$1.JSPType || new JSArray($1.buf)" js_unwrapImmutable/**/T/**/Array :: ByteArray# -> SomeTypedArray m T; {-# INLINE js_unwrapImmutable/**/T/**/Array #-};\
foreign import javascript unsafe "$1.JSPType || new JSArray($1.buf)" js_unwrap/**/T/**/Array :: MutableByteArray# s -> State# s -> (# State# s, SomeTypedArray m T #); {-# INLINE js_unwrap/**/T/**/Array #-};


CREATECONVERTERS(Int,i3,Int32,Int32Array,4)
CREATECONVERTERS(Int32,i3,Int32,Int32Array,4)
CREATECONVERTERS(Int16,i1,Int16,Int16Array,2)
CREATECONVERTERS(Int8,i8,Int8,Int8Array,1)
CREATECONVERTERS(Word,u3,Uint32,Uint32Array,4)
CREATECONVERTERS(Word32,u3,Uint32,Uint32Array,4)
CREATECONVERTERS(Word16,u1,Uint16,Uint16Array,2)
CREATECONVERTERS(Word8,u8,Uint8,Uint8Array,1)
CREATECONVERTERS(Float,f3,Float32,Float32Array,4)
CREATECONVERTERS(Double,f6,Float64,Float64Array,8)
CREATECONVERTERS(CChar,i8,Int8,Int8Array,1)
CREATECONVERTERS(CSChar,i8,Int8,Int8Array,1)
CREATECONVERTERS(CUChar,u8,Uint8,Uint8Array,1)
CREATECONVERTERS(CShort,i1,Int16,Int16Array,2)
CREATECONVERTERS(CUShort,u1,Uint16,Uint16Array,2)
CREATECONVERTERS(CInt,i3,Int32,Int32Array,4)
CREATECONVERTERS(CUInt,u3,Uint32,Uint32Array,4)
CREATECONVERTERS(CLong,i3,Int32,Int32Array,4)
CREATECONVERTERS(CULong,u3,Uint32,Uint32Array,4)
CREATECONVERTERS(CFloat,f3,Float32,Float32Array,4)
CREATECONVERTERS(CDouble,f6,Float64,Float64Array,8)
CREATECONVERTERS(Word8Clamped,uc,Uint8Clamped,Uint8ClampedArray,1)

-----------------------------------------------------------------------------
-- All immutable data functions
-----------------------------------------------------------------------------

#define JSTYPEDARRAY(T , JSName, JSArray, JSSize) \
foreign import javascript unsafe "new JSArray($1)" js_create/**/T/**/Array :: Int -> SomeTypedArray m T; {-# INLINE js_create/**/T/**/Array #-};\
foreign import javascript unsafe "new JSArray($1).fill($2)" js_fillNew/**/T/**/Array :: Int -> T -> SomeTypedArray m T; {-# INLINE js_fillNew/**/T/**/Array #-};\
foreign import javascript unsafe "var arr = h$fromListPrim($1); $r = new JSArray(arr.length); $r.set(arr);" js_fromList/**/T/**/Array :: Exts.Any -> SomeTypedArray m T; {-# INLINE js_fromList/**/T/**/Array #-};\
foreign import javascript unsafe "$r = new JSArray($1.length); $r.set($1);" js_fromArray/**/T/**/Array :: SomeTypedArray m0 t -> SomeTypedArray m T; {-# INLINE js_fromArray/**/T/**/Array #-};\
foreign import javascript unsafe "new JSArray($1)" js_view/**/T/**/Array :: SomeArrayBuffer m -> SomeTypedArray m T; {-# INLINE js_view/**/T/**/Array #-};\
foreign import javascript unsafe "$r = $1[$2]" js_index/**/T/**/Array :: SomeTypedArray m T -> Int -> T; {-# INLINE js_index/**/T/**/Array #-};\
foreign import javascript unsafe "$3.indexOf($2,$1)" js_indexOf/**/T/**/Array :: Int -> T -> SomeTypedArray m T -> Int; {-# INLINE js_indexOf/**/T/**/Array #-};\
foreign import javascript unsafe "$3.lastIndexOf($2,$1)" js_lastIndexOf/**/T/**/Array :: Int -> T -> SomeTypedArray m T -> Int; {-# INLINE js_lastIndexOf/**/T/**/Array #-};

JSTYPEDARRAY(Int,Int32,Int32Array,4)
JSTYPEDARRAY(Int32,Int32,Int32Array,4)
JSTYPEDARRAY(Int16,Int16,Int16Array,2)
JSTYPEDARRAY(Int8,Int8,Int8Array,1)
JSTYPEDARRAY(Word,Uint32,Uint32Array,4)
JSTYPEDARRAY(Word32,Uint32,Uint32Array,4)
JSTYPEDARRAY(Word16,Uint16,Uint16Array,2)
JSTYPEDARRAY(Word8,Uint8,Uint8Array,1)
JSTYPEDARRAY(Word8Clamped,Uint8Clamped,Uint8ClampedArray,1)
JSTYPEDARRAY(Float,Float32,Float32Array,4)
JSTYPEDARRAY(Double,Float64,Float64Array,8)
JSTYPEDARRAY(CChar,Int8,Int8Array,1)
JSTYPEDARRAY(CSChar,Int8,Int8Array,1)
JSTYPEDARRAY(CUChar,Uint8,Uint8Array,1)
JSTYPEDARRAY(CShort,Int16,Int16Array,2)
JSTYPEDARRAY(CUShort,Uint16,Uint16Array,2)
JSTYPEDARRAY(CInt,Int32,Int32Array,4)
JSTYPEDARRAY(CUInt,Uint32,Uint32Array,4)
JSTYPEDARRAY(CLong,Int32,Int32Array,4)
JSTYPEDARRAY(CULong,Uint32,Uint32Array,4)
JSTYPEDARRAY(CFloat,Float32,Float32Array,4)
JSTYPEDARRAY(CDouble,Float64,Float64Array,8)

-----------------------------------------------------------------------------
-- All DataView functions
-----------------------------------------------------------------------------

#define DATAVIEW(T, JSget, JSset, JSSize) \
foreign import javascript unsafe "$2.JSget($1)"      js_i_unsafeGet/**/T/**/BE  :: Int -> DataView -> T;{-# INLINE js_i_unsafeGet/**/T/**/BE #-};\
foreign import javascript unsafe "$2.JSget($1,true)" js_i_unsafeGet/**/T/**/LE  :: Int -> DataView -> T;{-# INLINE js_i_unsafeGet/**/T/**/LE #-};\
foreign import javascript safe   "$2.JSget($1)"      js_i_safeGet/**/T/**/BE    :: Int -> DataView -> T;{-# INLINE js_i_safeGet/**/T/**/BE #-};\
foreign import javascript safe   "$2.JSget($1,true)" js_i_safeGet/**/T/**/LE    :: Int -> DataView -> T;{-# INLINE js_i_safeGet/**/T/**/LE #-};\
foreign import javascript unsafe "$2.JSget($1)"      js_m_unsafeGet/**/T/**/BE  :: Int -> SomeDataView m -> State# s -> (# State# s, T #);{-# INLINE js_m_unsafeGet/**/T/**/BE #-};\
foreign import javascript unsafe "$2.JSget($1,true)" js_m_unsafeGet/**/T/**/LE  :: Int -> SomeDataView m -> State# s -> (# State# s, T #);{-# INLINE js_m_unsafeGet/**/T/**/LE #-};\
foreign import javascript safe   "$2.JSget($1)"      js_m_safeGet/**/T/**/BE    :: Int -> SomeDataView m -> State# s -> (# State# s, T #);{-# INLINE js_m_safeGet/**/T/**/BE #-};\
foreign import javascript safe   "$2.JSget($1,true)" js_m_safeGet/**/T/**/LE    :: Int -> SomeDataView m -> State# s -> (# State# s, T #);{-# INLINE js_m_safeGet/**/T/**/LE #-};\
foreign import javascript unsafe "$3.JSset($1,$2)"      js_unsafeSet/**/T/**/BE :: Int -> T -> SomeDataView m -> State# s -> (# State# s, () #);{-# INLINE js_unsafeSet/**/T/**/BE #-};\
foreign import javascript unsafe "$3.JSset($1,$2,true)" js_unsafeSet/**/T/**/LE :: Int -> T -> SomeDataView m -> State# s -> (# State# s, () #);{-# INLINE js_unsafeSet/**/T/**/LE #-};\
foreign import javascript safe   "$3.JSset($1,$2)"      js_safeSet/**/T/**/BE   :: Int -> T -> SomeDataView m -> State# s -> (# State# s, () #);{-# INLINE js_safeSet/**/T/**/BE #-};\
foreign import javascript safe   "$3.JSset($1,$2,true)" js_safeSet/**/T/**/LE   :: Int -> T -> SomeDataView m -> State# s -> (# State# s, () #);{-# INLINE js_safeSet/**/T/**/LE #-};

#define DATAVIEW8(T, JSget, JSset, JSSize) \
foreign import javascript unsafe "$2.JSget($1)"    js_i_unsafeGet/**/T :: Int -> DataView -> T;{-# INLINE js_i_unsafeGet/**/T #-};\
foreign import javascript safe   "$2.JSget($1)"    js_i_safeGet/**/T   :: Int -> DataView -> T;{-# INLINE js_i_safeGet/**/T #-};\
foreign import javascript unsafe "$2.JSget($1)"    js_m_unsafeGet/**/T :: Int -> SomeDataView m -> State# s -> (# State# s, T #);{-# INLINE js_m_unsafeGet/**/T #-};\
foreign import javascript safe   "$2.JSget($1)"    js_m_safeGet/**/T   :: Int -> SomeDataView m -> State# s -> (# State# s, T #);{-# INLINE js_m_safeGet/**/T #-};\
foreign import javascript unsafe "$3.JSset($1,$2)" js_unsafeSet/**/T   :: Int -> T -> SomeDataView m -> State# s -> (# State# s, () #);{-# INLINE js_unsafeSet/**/T #-};\
foreign import javascript safe   "$3.JSset($1,$2)" js_safeSet/**/T     :: Int -> T -> SomeDataView m -> State# s -> (# State# s, () #);{-# INLINE js_safeSet/**/T #-};


DATAVIEW(Int,getInt32,setInt32,4)
DATAVIEW(Int32,getInt32,setInt32,4)
DATAVIEW(Int16,getInt16,setInt16,2)
DATAVIEW(Word,getUint32,setUint32,4)
DATAVIEW(Word32,getUint32,setUint32,4)
DATAVIEW(Word16,getUint16,setUint16,2)
DATAVIEW(Float,getFloat32,setFloat32,4)
DATAVIEW(Double,getFloat64,setFloat64,8)
--DATAVIEW(CShort,getInt16,setInt16,2)
--DATAVIEW(CUShort,getUint16,setUint16,2)
--DATAVIEW(CInt,getInt32,setInt32,4)
--DATAVIEW(CUInt,getUint32,setUint32,4)
--DATAVIEW(CLong,getInt32,setInt32,4)
--DATAVIEW(CULong,getUint32,setUint32,4)
--DATAVIEW(CFloat,getFloat32,setFloat32,4)
--DATAVIEW(CDouble,getFloat64,setFloat64,8)


DATAVIEW8(Word8,getUint8,setUint8,1)
DATAVIEW8(Int8,getInt8,setInt8,1)
--DATAVIEW8(CChar,getInt8,setInt8,1)
--DATAVIEW8(CSChar,getInt8,setInt8,1)
--DATAVIEW8(CUChar,getUint8,setUint8,1)

-----------------------------------------------------------------------------
-- Misc
-----------------------------------------------------------------------------

seqList :: [a] -> [a]
seqList xs = go xs `seq` xs
  where go (x:ss) = x `seq` go ss
        go []     = ()
