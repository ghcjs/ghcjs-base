{-# LANGUAGE EmptyDataDecls, MagicHash, BangPatterns,
    CPP, ForeignFunctionInterface, JavaScriptFFI #-}

module GHCJS.Types ( JSRef(..)
                   , isNull
                   , isUndefined
                   , eqRef
                   , nullRef
                   , castRef
                   , JSString
                   , JSObject
                   , JSBool
                   , JSNumber
                   , JSArray
                   , JSFun
                   , mkRef
                   , Ref#
                   ) where

import GHC.Int
import GHC.Types
import GHC.Prim
import GHC.Ptr
import GHCJS.Prim
import Unsafe.Coerce

data JSBool_
data JSNumber_
data JSString_
data JSObject_ a
data JSArray_ a
data JSFun_ a

type JSBool     = JSRef JSBool_
type JSNumber   = JSRef JSNumber_
type JSString   = JSRef JSString_
type JSObject a = JSRef (JSObject_ a)
type JSFun a    = JSRef (JSFun_ a)
-- type JSObject'  = JSRef (JSObject (Any *))

type JSArray a  = JSRef (JSArray_ a)

#ifdef ghcjs_HOST_OS
type Ref# = ByteArray#

mkRef :: ByteArray# -> JSRef a
mkRef x = JSRef x
#else
type Ref# = Addr#

mkRef :: Addr# -> JSRef a
mkRef x = JSRef x
#endif

nullRef :: JSRef a
nullRef = js_nullRef
{-# INLINE nullRef #-}

castRef :: JSRef a -> JSRef b
castRef = unsafeCoerce
{-# INLINE castRef #-}

#ifdef ghcjs_HOST_OS
toPtr :: JSRef a -> Ptr b
toPtr (JSRef x) = unsafeCoerce (Ptr' x 0#)
{-# INLINE toPtr #-}

fromPtr :: Ptr a -> JSRef b
fromPtr p = let !(Ptr' x _) = unsafeCoerce p
            in  JSRef x
{-# INLINE fromPtr #-}

data Ptr' a = Ptr' ByteArray# Int#
#else
toPtr :: JSRef a -> Ptr b
toPtr = unsafeCoerce
{-# INLINE toPtr #-}

fromPtr :: Ptr a -> JSRef b
fromPtr = unsafeCoerce
{-# INLINE fromPtr #-}
#endif

foreign import javascript unsafe "$r = null"        js_nullRef     :: JSRef a

