{-# LANGUAGE EmptyDataDecls, MagicHash, BangPatterns,
    CPP, ForeignFunctionInterface, JavaScriptFFI #-}

module GHCJS.Types ( JSRef
                   , isNull
                   , isUndefined
                   , nullRef
                   , castRef
                   , JSString
--                   , JSObject
--                   , JSBool
--                   , JSNumber
--                   , JSFun
                   , mkRef
                   , Ref#
                   ) where

import Data.JSString.Internal.Type (JSString)

import GHC.Int
import GHC.Types
import GHC.Prim
import GHC.Ptr
import GHCJS.Prim

import Control.DeepSeq

import Unsafe.Coerce

instance NFData (JSRef a) where
  rnf x = x `seq` ()

-- fixme remove
data JSBool_
data JSNumber_
data JSObject_ a
-- data JSArray_ a
data JSFun_ a

type JSBool     = JSRef JSBool_
type JSNumber   = JSRef JSNumber_
type JSObject a = JSRef (JSObject_ a)
type JSFun a    = JSRef (JSFun_ a)

type Ref# = ByteArray#

mkRef :: ByteArray# -> JSRef a
mkRef x = JSRef x

nullRef :: JSRef a
nullRef = js_nullRef
{-# INLINE nullRef #-}

castRef :: JSRef a -> JSRef b
castRef = unsafeCoerce
{-# INLINE castRef #-}

toPtr :: JSRef a -> Ptr b
toPtr (JSRef x) = unsafeCoerce (Ptr' x 0#)
{-# INLINE toPtr #-}

fromPtr :: Ptr a -> JSRef b
fromPtr p = let !(Ptr' x _) = unsafeCoerce p
            in  JSRef x
{-# INLINE fromPtr #-}

data Ptr' a = Ptr' ByteArray# Int#

foreign import javascript unsafe "$r = null"        js_nullRef     :: JSRef a

