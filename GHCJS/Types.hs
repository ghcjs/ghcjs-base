{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module GHCJS.Types ( JSVal
                   , WouldBlockException(..)
                   , JSException(..)
                   , IsJSVal
                   , jsval
                   , isNull
                   , isUndefined
                   , nullRef
                   , JSString
                   , mkRef
                   , Ref#
                   , toPtr
                   , fromPtr
                   , JSRef
                   ) where

import Data.JSString.Internal.Type (JSString)
import GHCJS.Internal.Types

import GHC.JS.Prim

import GHC.Int
import GHC.Types
import GHC.Prim
import GHC.Ptr

import Control.DeepSeq

type Ref# = ByteArray#

mkRef :: ByteArray# -> JSVal
mkRef x = JSVal x

nullRef :: JSVal
nullRef = js_nullRef
{-# INLINE nullRef #-}

toPtr :: JSVal -> Ptr a
toPtr j = js_mkPtr j
{-# INLINE toPtr #-}

fromPtr :: Ptr a -> JSVal
fromPtr p = js_ptrVal p
{-# INLINE fromPtr #-}

foreign import javascript unsafe "((x) => { return null; })"
  js_nullRef :: JSVal

foreign import javascript unsafe "((x,y) => { return x; })"
  js_ptrVal  :: Ptr a -> JSVal

foreign import javascript unsafe "((x) => { h$ret1 = 0; return x; })"
  js_mkPtr :: JSVal -> Ptr a

-- | This is a deprecated copmatibility wrapper for the old JSRef type.
--
-- See https://github.com/ghcjs/ghcjs/issues/421
type JSRef a = JSVal
{-# DEPRECATED JSRef "Use JSVal instead, or a more specific newtype wrapper of JSVal " #-}
