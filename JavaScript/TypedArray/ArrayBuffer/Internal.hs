{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim, MagicHash, UnboxedTuples, MagicHash,
             TypeSynonymInstances, FlexibleInstances, DataKinds, PolyKinds
  #-}
module JavaScript.TypedArray.ArrayBuffer.Internal where

import GHCJS.Types

import GHCJS.Internal.Types
import GHCJS.Marshal.Pure

import GHC.Exts (State#)

newtype SomeArrayBuffer (a :: MutabilityType s) = SomeArrayBuffer (JSRef ())

type ArrayBuffer           = SomeArrayBuffer Immutable
type MutableArrayBuffer    = SomeArrayBuffer Mutable
type STArrayBuffer s       = SomeArrayBuffer (STMutable s)


instance PToJSRef MutableArrayBuffer where
  pToJSRef (SomeArrayBuffer b) = castRef b
instance PFromJSRef MutableArrayBuffer where
  pFromJSRef = SomeArrayBuffer . castRef

-- ----------------------------------------------------------------------------

foreign import javascript unsafe
  "$1.byteLength" js_byteLength :: SomeArrayBuffer any -> Int
foreign import javascript unsafe
  "new ArrayBuffer($1)" js_create :: Int -> State# s -> (# State# s, JSRef () #)
foreign import javascript unsafe
  "$2.slice($1)" js_slice1 :: Int -> JSRef () -> State# s -> (# State# s, JSRef () #)

-- ----------------------------------------------------------------------------
-- immutable non-IO slice

foreign import javascript unsafe
  "$2.slice($1)" js_slice1_imm :: Int -> SomeArrayBuffer any -> SomeArrayBuffer any
foreign import javascript unsafe
  "$3.slice($1,$2)" js_slice_imm :: Int -> Int -> SomeArrayBuffer any -> SomeArrayBuffer any
