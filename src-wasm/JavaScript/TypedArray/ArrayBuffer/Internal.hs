{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module JavaScript.TypedArray.ArrayBuffer.Internal where

import GHCJS.Types

import GHCJS.Internal.Types
import GHCJS.Marshal.Pure

import GHC.Exts (State#)

import Data.Typeable

newtype SomeArrayBuffer (a :: MutabilityType s) =
  SomeArrayBuffer JSVal deriving Typeable
instance IsJSVal (SomeArrayBuffer m)

type ArrayBuffer           = SomeArrayBuffer Immutable
type MutableArrayBuffer    = SomeArrayBuffer Mutable
type STArrayBuffer s       = SomeArrayBuffer (STMutable s)

instance PToJSVal MutableArrayBuffer where
  pToJSVal (SomeArrayBuffer b) = b
instance PFromJSVal MutableArrayBuffer where
  pFromJSVal = SomeArrayBuffer

-- ----------------------------------------------------------------------------

foreign import javascript unsafe
  "((x) => { return x.byteLength; })" js_byteLength :: SomeArrayBuffer any -> Int
foreign import javascript unsafe
  "((x) => { return new ArrayBuffer(x); })" js_create :: Int -> State# s -> (# State# s, JSVal #)
foreign import javascript unsafe
  "((x,y) => { return y.slice(x); })" js_slice1 :: Int -> JSVal -> State# s -> (# State# s, JSVal #)

-- ----------------------------------------------------------------------------
-- immutable non-IO slice

foreign import javascript unsafe
  "((x,y) => { return y.slice(x); })" js_slice1_imm :: Int -> SomeArrayBuffer any -> SomeArrayBuffer any
foreign import javascript unsafe
  "((x,y,z) => { return z.slice(x,y); })" js_slice_imm :: Int -> Int -> SomeArrayBuffer any -> SomeArrayBuffer any
