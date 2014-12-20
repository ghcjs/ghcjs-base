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
             BangPatterns
  #-}
{-
  experimental pure marshalling for lighter weight interaction in the quasiquoter
 -}
module GHCJS.Marshal.Pure ( PFromJSRef(..)
                          , PToJSRef(..)
                          ) where

import           Data.Char (chr, ord)
import           Data.Int (Int8, Int16, Int32)
import           Data.Maybe
import           Data.Text (Text)
import           Data.Word (Word8, Word16, Word32, Word)
#ifdef ghcjs_HOST_OS
import           Data.Bits ((.&.))
#endif
import           Unsafe.Coerce (unsafeCoerce)
import           GHC.Int
import           GHC.Word
import           GHC.Types
import           GHC.Float
import           GHC.Prim

import           GHCJS.Types
import           GHCJS.Foreign

class PToJSRef a where
  ptoJSRef :: a -> (JSRef a)

class PFromJSRef a where
  pfromJSRef :: JSRef a -> a

instance PFromJSRef (JSRef a) where pfromJSRef = castRef
                                    {-# INLINE pfromJSRef #-}
instance PFromJSRef ()        where pfromJSRef _ = ()
                                    {-# INLINE pfromJSRef #-}

instance PFromJSRef [Char] where pfromJSRef   = pfromJSRef_fromJSString
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Text   where pfromJSRef   = pfromJSRef_fromJSString
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Char   where pfromJSRef x = C# (jsrefToChar x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Bool   where pfromJSRef   = fromJSBool . castRef
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Int    where pfromJSRef x = I# (jsrefToInt x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Int8   where pfromJSRef x = I8# (jsrefToInt8 x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Int16  where pfromJSRef x = I16# (jsrefToInt16 x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Int32  where pfromJSRef x = I32# (jsrefToInt x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Word   where pfromJSRef x = W# (jsrefToWord x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Word8  where pfromJSRef x = W8# (jsrefToWord8 x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Word16 where pfromJSRef x = W16# (jsrefToWord16 x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Word32 where pfromJSRef x = W32# (jsrefToWord x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Float  where pfromJSRef x = F# (jsrefToFloat x)
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Double where pfromJSRef x = D# (jsrefToDouble x)
                                 {-# INLINE pfromJSRef #-}

instance PFromJSRef a => PFromJSRef (Maybe a) where
    pfromJSRef x | isUndefined x || isNull x = Nothing
    pfromJSRef x = Just (pfromJSRef (castRef x))
    {-# INLINE pfromJSRef #-}

instance PToJSRef (JSRef a) where ptoJSRef = castRef
                                  {-# INLINE ptoJSRef #-}

instance PToJSRef [Char] where ptoJSRef         = ptoJSRef_toJSString
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Text   where ptoJSRef          = ptoJSRef_toJSString
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Char   where ptoJSRef (C# c)   = charToJSRef c
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Bool   where ptoJSRef True     = castRef jsTrue
                               ptoJSRef False    = castRef jsFalse
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Int    where ptoJSRef (I# x)   = intToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Int8   where ptoJSRef (I8# x)  = intToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Int16  where ptoJSRef (I16# x) = intToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Int32  where ptoJSRef (I32# x) = intToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Word   where ptoJSRef (W# x)   = wordToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Word8  where ptoJSRef (W8# x)  = wordToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Word16 where ptoJSRef (W16# x) = wordToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Word32 where ptoJSRef (W32# x) = wordToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Float  where ptoJSRef (F# x)   = floatToJSRef x
                               {-# INLINE ptoJSRef #-}
instance PToJSRef Double where ptoJSRef (D# x)   = doubleToJSRef x
                               {-# INLINE ptoJSRef #-}

instance PToJSRef a => PToJSRef (Maybe a) where
    ptoJSRef Nothing  = jsNull
    ptoJSRef (Just a) = castRef (ptoJSRef a)
    {-# INLINE ptoJSRef #-}

ptoJSRef_toJSString :: ToJSString a => a -> (JSRef a)
ptoJSRef_toJSString = castRef . toJSString
{-# INLINE ptoJSRef_toJSString #-}

pfromJSRef_fromJSString :: FromJSString a => JSRef a -> a
pfromJSRef_fromJSString = fromJSString . castRef
{-# INLINE pfromJSRef_fromJSString #-}

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$r = $1|0;"          jsrefToWord   :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1&0xff;"       jsrefToWord8  :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1&0xffff;"     jsrefToWord16 :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1|0;"          jsrefToInt    :: JSRef a -> Int#
foreign import javascript unsafe "$r = $1&0xff;"       jsrefToInt8   :: JSRef a -> Int#
foreign import javascript unsafe "$r = $1&0xffff;"     jsrefToInt16  :: JSRef a -> Int#
foreign import javascript unsafe "$r = +$1;"           jsrefToFloat  :: JSRef a -> Float#
foreign import javascript unsafe "$r = +$1;"           jsrefToDouble :: JSRef a -> Double#
foreign import javascript unsafe "$r = $1&0x7fffffff;" jsrefToChar   :: JSRef a -> Char#

foreign import javascript unsafe "$r = $1;" wordToJSRef   :: Word#   -> JSRef a
foreign import javascript unsafe "$r = $1;" intToJSRef    :: Int#    -> JSRef a
foreign import javascript unsafe "$r = $1;" doubleToJSRef :: Double# -> JSRef a
foreign import javascript unsafe "$r = $1;" floatToJSRef  :: Float#  -> JSRef a
foreign import javascript unsafe "$r = $1;" charToJSRef   :: Char#   -> JSRef a
#else
jsrefToWord   :: JSRef a -> Word#
jsrefToWord r   = let !(W# x) = unsafeCoerce r .&. 0xffffffff in x
jsrefToWord8  :: JSRef a -> Word#
jsrefToWord8 r  = let !(W# x) = unsafeCoerce r .&. 0xff in x
jsrefToWord16 :: JSRef a -> Word#
jsrefToWord16 r = let !(W# x) = unsafeCoerce r .&. 0xffff in x
jsrefToInt    :: JSRef a -> Int#
jsrefToInt r    = let !(I# x) = unsafeCoerce r .&. 0xffffffff in x
jsrefToInt8   :: JSRef a -> Int#
jsrefToInt8 r   = let !(I# x) = unsafeCoerce r .&. 0xff in x
jsrefToInt16  :: JSRef a -> Int#
jsrefToInt16 r  = let !(I# x) = unsafeCoerce r .&. 0xffff in x
jsrefToFloat  :: JSRef a -> Float#
jsrefToFloat r  = let !(F# x) = unsafeCoerce r in x
jsrefToDouble :: JSRef a -> Double#
jsrefToDouble r = let !(D# x) = unsafeCoerce r in x
jsrefToChar   :: JSRef a -> Int#
jsrefToChar r   = let !(C# x) = chr (ord (unsafeCoerce r) .&. 0x7fffffff) in x

wordToJSRef   :: Word# -> JSRef a
wordToJSRef x   = unsafeCoerce (W# x)
intToJSRef    :: Int# -> JSRef a
intToJSRef x    = unsafeCoerce (I# x)
doubleToJSRef :: Double# -> JSRef a
doubleToJSRef x = unsafeCoerce (D# x)
floatToJSRef  :: Float# -> JSRef a
floatToJSRef  x = unsafeCoerce (F# x)
#endif


