{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module JavaScript.TypedArray.DataView.Internal where

import Data.Int
import Data.Typeable
import Data.Word

import GHC.Exts ( State# )

import GHC.JS.Prim
import GHCJS.Internal.Types

import JavaScript.TypedArray.ArrayBuffer.Internal

newtype SomeDataView (a :: MutabilityType s) = SomeDataView JSVal
  deriving Typeable

type DataView        = SomeDataView Immutable
type MutableDataView = SomeDataView Mutable
type STDataView s    = SomeDataView (STMutable s)

#define JSU foreign import javascript unsafe
#define JSS foreign import javascript safe

JSU "((x) => { return new DataView(x); })"
    js_dataView1 :: JSVal -> JSVal
JSS "((x,y) => { return new DataView(y,x); })"
    js_dataView2 :: Int -> JSVal -> SomeDataView m
JSU "((x,y) => { return new DataView(y,x); })"
    js_unsafeDataView2 :: Int -> JSVal-> SomeDataView m
JSS "((x,y,z) => { return new DataView(z,x,y); })"
    js_dataView :: Int -> Int -> JSVal -> SomeDataView m
JSU "((x,y,z) => { return new DataView(z,x,y); })" 
    js_unsafeDataView :: Int -> Int -> JSVal -> JSVal
JSU "((x) => { return new DataView(x.buffer.slice(x.byteOffset, x.byteLength)); })"
    js_cloneDataView :: SomeDataView m -> IO (SomeDataView m1)

-- ----------------------------------------------------------------------------
-- immutable getters

JSU "((x,y) => { return y.getInt8(x); })"          js_i_unsafeGetInt8       :: Int -> DataView -> Int8
JSU "((x,y) => { return y.getUint8(x); })"         js_i_unsafeGetUint8      :: Int -> DataView -> Word8
JSU "((x,y) => { return y.getInt16(x); })"         js_i_unsafeGetInt16BE    :: Int -> DataView -> Int16
JSU "((x,y) => { return y.getInt32(x); })"         js_i_unsafeGetInt32BE    :: Int -> DataView -> Int
JSU "((x,y) => { return y.getUint16(x); })"        js_i_unsafeGetUint16BE   :: Int -> DataView -> Word16
JSU "((x,y) => { return y.getUint32(x)|0; })"      js_i_unsafeGetUint32BE   :: Int -> DataView -> Word
JSU "((x,y) => { return y.getFloat32(x); })"       js_i_unsafeGetFloat32BE  :: Int -> DataView -> Double
JSU "((x,y) => { return y.getFloat64(x); })"       js_i_unsafeGetFloat64BE  :: Int -> DataView -> Double
JSU "((x,y) => { return y.getInt16(x,true); })"    js_i_unsafeGetInt16LE    :: Int -> DataView -> Int16
JSU "((x,y) => { return y.getInt32(x,true); })"    js_i_unsafeGetInt32LE    :: Int -> DataView -> Int
JSU "((x,y) => { return y.getUint16(x,true); })"   js_i_unsafeGetUint16LE   :: Int -> DataView -> Word16
JSU "((x,y) => { return y.getUint32(x,true)|0; })" js_i_unsafeGetUint32LE   :: Int -> DataView -> Word
JSU "((x,y) => { return y.getFloat32(x,true); })"  js_i_unsafeGetFloat32LE  :: Int -> DataView -> Double
JSU "((x,y) => { return y.getFloat64(x,true); })"  js_i_unsafeGetFloat64LE  :: Int -> DataView -> Double

JSS "((x,y) => { return y.getInt8(x); })"          js_i_getInt8       :: Int -> DataView -> Int8
JSS "((x,y) => { return y.getUint8(x); })"         js_i_getUint8      :: Int -> DataView -> Word8
JSS "((x,y) => { return y.getInt16(x); })"         js_i_getInt16BE    :: Int -> DataView -> Int16
JSS "((x,y) => { return y.getInt32(x); })"         js_i_getInt32BE    :: Int -> DataView -> Int
JSS "((x,y) => { return y.getUint16(x); })"        js_i_getUint16BE   :: Int -> DataView -> Word16
JSS "((x,y) => { return y.getUint32(x)|0; })"      js_i_getUint32BE   :: Int -> DataView -> Word
JSS "((x,y) => { return y.getFloat32(x); })"       js_i_getFloat32BE  :: Int -> DataView -> Double
JSS "((x,y) => { return y.getFloat64(x); })"       js_i_getFloat64BE  :: Int -> DataView -> Double
JSS "((x,y) => { return y.getInt16(x,true); })"    js_i_getInt16LE    :: Int -> DataView -> Int16
JSS "((x,y) => { return y.getInt32(x,true); })"    js_i_getInt32LE    :: Int -> DataView -> Int
JSS "((x,y) => { return y.getUint16(x,true); })"   js_i_getUint16LE   :: Int -> DataView -> Word16
JSS "((x,y) => { return y.getUint32(x,true)|0; })" js_i_getUint32LE   :: Int -> DataView -> Word
JSS "((x,y) => { return y.getFloat32(x,true); })"  js_i_getFloat32LE  :: Int -> DataView -> Double
JSS "((x,y) => { return y.getFloat64(x,true); })"  js_i_getFloat64LE  :: Int -> DataView -> Double

-- ----------------------------------------------------------------------------
-- mutable getters

JSU "((x,y) => { return y.getInt8(x); })"          js_m_unsafeGetInt8      :: Int -> SomeDataView m -> State# s -> (# State# s, Int8   #)
JSU "((x,y) => { return y.getUint8(x); })"         js_m_unsafeGetUint8     :: Int -> SomeDataView m -> State# s -> (# State# s, Word8  #)
JSU "((x,y) => { return y.getInt16(x); })"         js_m_unsafeGetInt16BE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
JSU "((x,y) => { return y.getInt32(x); })"         js_m_unsafeGetInt32BE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
JSU "((x,y) => { return y.getUint16(x); })"        js_m_unsafeGetUint16BE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
JSU "((x,y) => { return y.getUint32(x)|0; })"      js_m_unsafeGetUint32BE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
JSU "((x,y) => { return y.getFloat32(x); })"       js_m_unsafeGetFloat32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSU "((x,y) => { return y.getFloat64(x); })"       js_m_unsafeGetFloat64BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSU "((x,y) => { return y.getInt16(x,true); })"    js_m_unsafeGetInt16LE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
JSU "((x,y) => { return y.getInt32(x,true); })"    js_m_unsafeGetInt32LE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
JSU "((x,y) => { return y.getUint16(x,true); })"   js_m_unsafeGetUint16LE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
JSU "((x,y) => { return y.getUint32(x,true)|0; })" js_m_unsafeGetUint32LE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
JSU "((x,y) => { return y.getFloat32(x,true); })"  js_m_unsafeGetFloat32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSU "((x,y) => { return y.getFloat64(x,true); })"  js_m_unsafeGetFloat64LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)

JSS "((x,y) => { return y.getInt8(x); })"          js_m_getInt8            :: Int -> SomeDataView m -> State# s -> (# State# s, Int8   #)
JSS "((x,y) => { return y.getUint8(x); })"         js_m_getUint8           :: Int -> SomeDataView m -> State# s -> (# State# s, Word8  #)
JSS "((x,y) => { return y.getInt16(x); })"         js_m_getInt16BE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
JSS "((x,y) => { return y.getInt32(x); })"         js_m_getInt32BE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
JSS "((x,y) => { return y.getUint16(x); })"        js_m_getUint16BE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
JSS "((x,y) => { return y.getUint32(x)|0; })"      js_m_getUint32BE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
JSS "((x,y) => { return y.getFloat32(x); })"       js_m_getFloat32BE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSS "((x,y) => { return y.getFloat64(x); })"       js_m_getFloat64BE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSS "((x,y) => { return y.getInt16(x,true); })"    js_m_getInt16LE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
JSS "((x,y) => { return y.getInt32(x,true); })"    js_m_getInt32LE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
JSS "((x,y) => { return y.getUint16(x,true); })"   js_m_getUint16LE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
JSS "((x,y) => { return y.getUint32(x,true)|0; })" js_m_getUint32LE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
JSS "((x,y) => { return y.getFloat32(x,true); })"  js_m_getFloat32LE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSS "((x,y) => { return y.getFloat64(x,true); })"  js_m_getFloat64LE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)

-- ----------------------------------------------------------------------------
-- mutable setters

JSU "((x,y,z) => { x.setInt8(x,y); })"         js_unsafeSetInt8      :: Int -> Int8   -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint8(x,y); })"        js_unsafeSetUint8     :: Int -> Word8  -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setInt16(x,y); })"        js_unsafeSetInt16BE   :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setInt32(x,y); })"        js_unsafeSetInt32BE   :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint16(x,y); })"       js_unsafeSetUint16BE  :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint32(x,y); })"       js_unsafeSetUint32BE  :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setFloat32(x,y); })"      js_unsafeSetFloat32BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setFloat64(x,y); })"      js_unsafeSetFloat64BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setInt16(x,y,true); })"   js_unsafeSetInt16LE   :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setInt32(x,y,true); })"   js_unsafeSetInt32LE   :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint16(x,y,true); })"  js_unsafeSetUint16LE  :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint32(x,y,true); })"  js_unsafeSetUint32LE  :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setFloat32(x,y,true); })" js_unsafeSetFloat32LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setFloat64(x,y,true); })" js_unsafeSetFloat64LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)

JSS "((x,y,z) => { x.setInt8(x,y); })"         js_setInt8            :: Int -> Int8   -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint8(x,y); })"        js_setUint8           :: Int -> Word8  -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setInt16(x,y); })"        js_setInt16BE         :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setInt32(x,y); })"        js_setInt32BE         :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint16(x,y); })"       js_setUint16BE        :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint32(x,y); })"       js_setUint32BE        :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setFloat32(x,y); })"      js_setFloat32BE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setFloat64(x,y); })"      js_setFloat64BE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setInt16(x,y,true); })"   js_setInt16LE         :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setInt32(x,y,true); })"   js_setInt32LE         :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint16(x,y,true); })"  js_setUint16LE        :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint32(x,y,true); })"  js_setUint32LE        :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setFloat32(x,y,true); })" js_setFloat32LE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setFloat64(x,y,true); })" js_setFloat64LE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)

