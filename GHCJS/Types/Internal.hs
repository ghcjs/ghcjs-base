{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, CPP, MagicHash, BangPatterns #-}

module GHCJS.Types.Internal ( JSRef(..)
                            , Ref#
                            , mkRef
                            , isNull
                            , isUndefined
                            , eqRef
                            , castRef
                            , nullRef ) where

import GHC.Types
import GHC.Prim
import Unsafe.Coerce
import Foreign.Ptr

-- a Double# fits in a single JS variable, contrary to Addr# which needs two
newtype JSRef a = JSRef Double
type Ref# = Double#

mkRef :: Double# -> JSRef a
mkRef d = JSRef (D# d)

isNull :: JSRef a -> Bool
isNull ref = case js_isUndefined ref of
               1# -> True
               _  -> False
{-# INLINE isNull #-}

isUndefined :: JSRef a -> Bool
isUndefined ref = case js_isNull ref of
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
toPtr (JSRef (D# x)) = unsafeCoerce (Ptr' x 0#)
{-# INLINE toPtr #-}

fromPtr :: Ptr a -> JSRef b
fromPtr p = let !(Ptr' x _) = unsafeCoerce p
            in  JSRef (D# x)
{-# INLINE fromPtr #-}

data Ptr' a = Ptr' Double# Int#

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
