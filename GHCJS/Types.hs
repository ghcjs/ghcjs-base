{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module GHCJS.Types ( JSVal
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
                   ) where

import Data.JSString.Internal.Type (JSString)
import GHCJS.Internal.Types

import GHCJS.Prim

import GHC.Int
import GHC.Types
import GHC.Prim
import GHC.Ptr

import Control.DeepSeq
import Unsafe.Coerce

type Ref# = ByteArray#

mkRef :: ByteArray# -> JSVal
mkRef x = JSVal x

nullRef :: JSVal
nullRef = js_nullRef
{-# INLINE nullRef #-}

toPtr :: JSVal -> Ptr a
toPtr (JSVal x) = unsafeCoerce (Ptr' x 0#)
{-# INLINE toPtr #-}

fromPtr :: Ptr a -> JSVal
fromPtr p = js_ptrVal p
{-# INLINE fromPtr #-}

data Ptr' a = Ptr' ByteArray# Int#

foreign import javascript unsafe "$r = null;"
  js_nullRef :: JSVal

foreign import javascript unsafe "$r = $1_1;"
  js_ptrVal  :: Ptr a -> JSVal

foreign import javascript unsafe "$r1 = $1; $r2 = 0;"
  js_mkPtr :: JSVal -> Ptr a
