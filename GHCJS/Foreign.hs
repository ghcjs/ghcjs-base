{-# LANGUAGE ForeignFunctionInterface, CPP, MagicHash, FlexibleInstances, BangPatterns #-}

{-
  Utilities for interaction with JavaScript objects.
  Objects are stored in an Addr# primitive, where the
  second (offset) field is unused.
-}

module GHCJS.Foreign ( ToJSString(..)
                     , FromJSString(..)
                     , Ptr'(..)
                     , fromPtr
                     , toPtr) where

import GHCJS.Types

import qualified Data.Text as T
import Foreign.Ptr
import Unsafe.Coerce
import GHC.Prim
import GHC.Exts

import qualified Data.Text.Array as A

#if __GHCJS__==1
foreign import javascript unsafe "$r = h$toStr($1,$1_2,$2); $r2 = 0;"
  js_toString :: Ptr () -> Int -> Ptr ()

foreign import javascript unsafe "$r = h$fromStr($1); $r2 = h$ret1;"
  js_fromString :: Ptr () -> Ptr ()
#else
js_toString :: Ptr () -> Int -> Ptr ()
js_toString = error "only available in JavaScript"

js_fromString :: Ptr () -> Ptr ()
js_fromString = error "only available in JavaScript"
#endif

class ToJSString a where
  toJSString :: a -> JSString

class FromJSString a where
  fromJSString :: JSString -> a

instance ToJSString [Char] where
  toJSString = toJSString . T.pack

instance FromJSString [Char] where
  fromJSString = T.unpack . fromJSString

instance ToJSString T.Text where
  toJSString t =
    let !(Text' (Array' b) (I# offset) length) = unsafeCoerce t
    in  castPtr $ (js_toString (toPtr $ Ptr' b offset) length)

instance FromJSString T.Text where
  fromJSString p =
    let !(Ptr' ba l) = fromPtr (js_fromString $ castPtr p)
    in  unsafeCoerce (Text' (Array' ba) 0 (I# l))

-- something that we can unsafeCoerce Text from
data Text' = Text'
    {-# UNPACK #-} !Array'           -- payload
    {-# UNPACK #-} !Int              -- offset
    {-# UNPACK #-} !Int              -- length

data Array' = Array' {
      aBA :: ByteArray#
    }

-- same rep as Ptr Addr#, use this to get just the first field out
fromPtr :: Ptr a -> Ptr' b
fromPtr = unsafeCoerce

toPtr :: Ptr' a -> Ptr b
toPtr = unsafeCoerce

data Ptr' a = Ptr' ByteArray# Int#
