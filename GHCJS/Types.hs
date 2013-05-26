{-# LANGUAGE EmptyDataDecls, MagicHash, BangPatterns, CPP, UnliftedFFITypes #-}

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
                   , mkRef
                   , Ref#
                   )
                 where

import GHC.Int
import GHC.Types
import GHC.Prim
import GHC.Ptr
import GHCJS.Prim
import Unsafe.Coerce

-- a ByteArray# fits in a single JS variable, contrary to Addr# which needs two
-- data JSRef a = JSRef ByteArray#

data JSBool_
data JSNumber_
data JSString_
data JSObject_ a
data JSArray_ a

type JSBool     = JSRef JSBool_
type JSNumber   = JSRef JSNumber_
type JSString   = JSRef JSString_
type JSObject a = JSRef (JSObject_ a)
-- type JSObject'  = JSRef (JSObject (Any *))

type JSArray a  = JSRef (JSArray_ a)

type Ref# = ByteArray#

mkRef :: ByteArray# -> JSRef a
mkRef x = JSRef x

isNull :: JSRef a -> Bool
isNull ref = case js_isNull ref of
               1# -> True
               _  -> False
{-# INLINE isNull #-}

isUndefined :: JSRef a -> Bool
isUndefined ref = case js_isUndefined ref of
                    1# -> True
                    _  -> False
{-# INLINE isUndefined #-}

eqRef :: JSRef a -> JSRef b -> Bool
eqRef x y = case js_eqRef x y of
              1# -> True
              _  -> False
{-# INLINE eqRef #-}

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

#ifdef __GHCJS__
foreign import javascript unsafe "$1 === null ? 1 : 0"      js_isNull      :: JSRef a -> Int#
foreign import javascript unsafe "$1 === undefined ? 1 : 0" js_isUndefined :: JSRef a -> Int#
foreign import javascript unsafe "$1 === $2 ? 1 : 0"        js_eqRef       :: JSRef a -> JSRef b -> Int#
foreign import javascript unsafe "$r = null"                js_nullRef     :: JSRef a
#else
js_isNull :: JSRef a -> Int#
js_isNull = error "js_isNull: only available in JavaScript"

js_isUndefined :: JSRef a -> Int#
js_isUndefined = error "js_isUndefined: only available in JavaScript"

js_eqRef :: JSRef a -> JSRef b -> Int#
js_eqRef = error "js_eqRef: only available in JavaScript"

js_nullRef :: JSRef a
js_nullRef = error "js_nullRef: only available in JavaScript"
#endif


