{-# LANGUAGE CPP,
             DefaultSignatures,
             TypeOperators,
             ScopedTypeVariables,
             DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
             TupleSections,
             MagicHash,
             JavaScriptFFI,
             ForeignFunctionInterface,
             UnliftedFFITypes,
             BangPatterns,
             TypeFamilies,
             DataKinds,
             DeriveDataTypeable
  #-}
{-
  experimental pure marshalling for lighter weight interaction in the quasiquoter
 -}
module GHCJS.Marshal.Pure ( PFromJSRef(..)
                          , PToJSRef(..)
                          ) where

import           Data.Char (chr, ord)
import           Data.Data
import           Data.Int (Int8, Int16, Int32)
import           Data.JSString.Internal.Type
import           Data.Maybe
import           Data.Text (Text)
import           Data.Typeable
import           Data.Word (Word8, Word16, Word32, Word)
import           Data.JSString
import           Data.JSString.Text
import           Data.Bits ((.&.))
import           Unsafe.Coerce (unsafeCoerce)
import           GHC.Int
import           GHC.Word
import           GHC.Types
import           GHC.Float
import           GHC.Prim

import           GHCJS.Types
import qualified GHCJS.Prim as Prim
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal.Internal

{-
type family IsPureShared a where
  IsPureShared PureExclusive = False
  IsPureShared PureShared    = True

type family IsPureExclusive a where
  IsPureExclusive PureExclusive = True
  IsPureExclusive PureShared    = True
  -}

instance PFromJSRef (JSRef a) where pFromJSRef = castRef
                                    {-# INLINE pFromJSRef #-}
instance PFromJSRef ()        where pFromJSRef _ = ()
                                    {-# INLINE pFromJSRef #-}

instance PFromJSRef [Char] where pFromJSRef   = Prim.fromJSString
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Text   where pFromJSRef   = textFromJSRef
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Char   where pFromJSRef x = C# (jsrefToChar x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Bool   where pFromJSRef   = isTruthy -- fromJSBool . castRef
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Int    where pFromJSRef x = I# (jsrefToInt x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Int8   where pFromJSRef x = I8# (jsrefToInt8 x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Int16  where pFromJSRef x = I16# (jsrefToInt16 x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Int32  where pFromJSRef x = I32# (jsrefToInt x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Word   where pFromJSRef x = W# (jsrefToWord x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Word8  where pFromJSRef x = W8# (jsrefToWord8 x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Word16 where pFromJSRef x = W16# (jsrefToWord16 x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Word32 where pFromJSRef x = W32# (jsrefToWord x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Float  where pFromJSRef x = F# (jsrefToFloat x)
                                 {-# INLINE pFromJSRef #-}
instance PFromJSRef Double where pFromJSRef x = D# (jsrefToDouble x)
                                 {-# INLINE pFromJSRef #-}

instance PFromJSRef a => PFromJSRef (Maybe a) where
    pFromJSRef x | isUndefined x || isNull x = Nothing
    pFromJSRef x = Just (pFromJSRef (castRef x))
    {-# INLINE pFromJSRef #-}

instance PToJSRef (JSRef a) where pToJSRef = castRef
                                  {-# INLINE pToJSRef #-}
instance PToJSRef JSString  where pToJSRef          = castRef . unJSString
                                  {-# INLINE pToJSRef #-}
instance PToJSRef [Char]    where pToJSRef          = Prim.toJSString
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Text      where pToJSRef          = castRef . unJSString . textToJSString
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Char      where pToJSRef (C# c)   = charToJSRef c
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Bool      where pToJSRef True     = castRef jsTrue
                                  pToJSRef False    = castRef jsFalse
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Int       where pToJSRef (I# x)   = intToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Int8      where pToJSRef (I8# x)  = intToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Int16     where pToJSRef (I16# x) = intToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Int32     where pToJSRef (I32# x) = intToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Word      where pToJSRef (W# x)   = wordToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Word8     where pToJSRef (W8# x)  = wordToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Word16    where pToJSRef (W16# x) = wordToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Word32    where pToJSRef (W32# x) = wordToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Float     where pToJSRef (F# x)   = floatToJSRef x
                                  {-# INLINE pToJSRef #-}
instance PToJSRef Double    where pToJSRef (D# x)   = doubleToJSRef x
                                  {-# INLINE pToJSRef #-}

instance PToJSRef a => PToJSRef (Maybe a) where
    pToJSRef Nothing  = jsNull
    pToJSRef (Just a) = castRef (pToJSRef a)
    {-# INLINE pToJSRef #-}

foreign import javascript unsafe "$r = $1|0;"          jsrefToWord   :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1&0xff;"       jsrefToWord8  :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1&0xffff;"     jsrefToWord16 :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1|0;"          jsrefToInt    :: JSRef a -> Int#
foreign import javascript unsafe "$r = $1<<24>>24;"    jsrefToInt8   :: JSRef a -> Int#
foreign import javascript unsafe "$r = $1<<16>>16;"    jsrefToInt16  :: JSRef a -> Int#
foreign import javascript unsafe "$r = +$1;"           jsrefToFloat  :: JSRef a -> Float#
foreign import javascript unsafe "$r = +$1;"           jsrefToDouble :: JSRef a -> Double#
foreign import javascript unsafe "$r = $1&0x7fffffff;" jsrefToChar   :: JSRef a -> Char#

foreign import javascript unsafe "$r = $1;" wordToJSRef   :: Word#   -> JSRef a
foreign import javascript unsafe "$r = $1;" intToJSRef    :: Int#    -> JSRef a
foreign import javascript unsafe "$r = $1;" doubleToJSRef :: Double# -> JSRef a
foreign import javascript unsafe "$r = $1;" floatToJSRef  :: Float#  -> JSRef a
foreign import javascript unsafe "$r = $1;" charToJSRef   :: Char#   -> JSRef a

