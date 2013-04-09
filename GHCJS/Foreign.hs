{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, CPP, MagicHash, FlexibleInstances, BangPatterns #-}

module GHCJS.Foreign ( ToJSString(..)
                     , FromJSString(..)
                     , mvarRef
                     , fromJSBool
                     , fromJSBool'
                     , toJSBool
                     , jsTrue
                     , jsFalse
                     , jsNull
                     , jsUndefined
                     ) where

import           GHCJS.Types
import           GHCJS.Types.Internal

import           GHC.Prim
import           GHC.Exts

import           Control.Concurrent.MVar
import qualified Data.Text as T
import           Foreign.Ptr
import           Unsafe.Coerce


import qualified Data.Text.Array as A

#ifdef __GHCJS__
foreign import javascript unsafe "$r = h$toStr($1,$2,$3);" js_toString :: Ref# -> Int# -> Int# -> Ref#
foreign import javascript unsafe "$r = h$fromStr($1); $r2 = h$ret1;" js_fromString :: Ref# -> Ptr ()
foreign import javascript unsafe "($1 === true) ? 1 : 0" js_fromBool :: JSBool -> Int#
foreign import javascript unsafe "$1 ? 1 : 0" js_isTruthy :: JSRef a -> Int#
foreign import javascript unsafe "$r = true"  js_true :: Int# -> Ref#
foreign import javascript unsafe "$r = false" js_false :: Int# -> Ref#
foreign import javascript unsafe "$r = null"  js_null :: Int# -> Ref#
foreign import javascript unsafe "$r = undefined"  js_undefined :: Int# -> Ref#
#else
js_toString :: Ref# -> Int# -> Int# -> Ref#
js_toString = error "js_toString: only available in JavaScript"
js_fromString :: Ref# -> Ptr ()
js_fromString = error "js_fromString: only available in JavaScript"
js_fromBool :: JSBool -> Int#
js_fromBool = error "js_fromBool: only available in JavaScript"
js_isTruthy :: JSRef a -> Int#
js_isTruthy = error "js_isTruthy: only available in JavaScript"
js_true :: Int# -> Ref#
js_true = error "js_true: only available in JavaScript"
js_false :: Int# -> Ref#
js_false = error "js_false: only available in JavaScript"
js_null :: Int# -> Ref#
js_null = error "js_null: only available in JavaScript"
js_undefined :: Int# -> Ref#
js_undefined = error "js_undefined: only available in JavaScript"
#endif

class ToJSString a where
  toJSString :: a -> JSString

class FromJSString a where
  fromJSString :: JSString -> a

instance ToJSString [Char] where
  toJSString = toJSString . T.pack
  {-# INLINE toJSString #-}

instance FromJSString [Char] where
  fromJSString = T.unpack . fromJSString
  {-# INLINE fromJSString #-}

instance ToJSString T.Text where
  toJSString t =
    let !(Text'' (Array'' b) (I# offset) (I# length)) = unsafeCoerce t
    in  mkRef (js_toString b offset length)
  {-# INLINE toJSString #-}
  
instance FromJSString T.Text where
  fromJSString (JSRef (D# ref)) =
    let !(Ptr' ba l) = ptrToPtr' (js_fromString ref)
    in  unsafeCoerce (Text' (Array' ba) 0 (I# l))
  {-# INLINE fromJSString #-}
fromJSBool :: JSBool -> Bool
fromJSBool b = case js_fromBool b of
                 1# -> True
                 _  -> False
{-# INLINE fromJSBool #-}

toJSBool :: Bool -> JSBool
toJSBool True = jsTrue
toJSBool _    = jsFalse
{-# INLINE toJSBool #-}

-- check whether a reference is `truthy' in the JavaScript sense
fromJSBool' :: JSRef a -> Bool
fromJSBool' b = case js_isTruthy b of
                  1# -> True
                  _  -> False
{-# INLINE fromJSBool' #-}

jsTrue :: JSBool
jsTrue = mkRef (js_true 0#)

jsFalse :: JSBool
jsFalse = mkRef (js_false 0#)

jsNull :: JSRef a
jsNull = mkRef (js_null 0#)

jsUndefined :: JSRef a
jsUndefined = mkRef (js_undefined 0#)

mvarRef :: MVar a -> JSObject (MVar a)
mvarRef = unsafeCoerce

-- something that we can unsafeCoerce Text from
data Text' = Text'
    {-# UNPACK #-} !Array'           -- payload
    {-# UNPACK #-} !Int              -- offset
    {-# UNPACK #-} !Int              -- length

data Array' = Array' {
      aBA :: ByteArray#
    }

data Text'' = Text''
    {-# UNPACK #-} !Array''          -- payload
    {-# UNPACK #-} !Int              -- offset
    {-# UNPACK #-} !Int              -- length

data Array'' = Array'' {
      aRef :: Ref#
    }

-- same rep as Ptr Addr#, use this to get just the first field out
data Ptr' a = Ptr' ByteArray# Int#

ptrToPtr' :: Ptr a -> Ptr' b
ptrToPtr' = unsafeCoerce

ptr'ToPtr :: Ptr' a -> Ptr b
ptr'ToPtr = unsafeCoerce

