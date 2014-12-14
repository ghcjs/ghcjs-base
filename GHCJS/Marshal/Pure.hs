{-# LANGUAGE DefaultSignatures,
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

  ptoJSRefListOf :: [a] -> (JSRef [a])
  ptoJSRefListOf = error "ptoJSRefListOf"

class PFromJSRef a where
  pfromJSRef :: JSRef a -> a

  pfromJSRefListOf :: JSRef [a] -> [a]
  pfromJSRefListOf = error "ptoJSRefListOf"

instance PFromJSRef (JSRef a) where pfromJSRef = castRef
                                    {-# INLINE pfromJSRef #-}
instance PFromJSRef ()        where pfromJSRef _ = ()
                                    {-# INLINE pfromJSRef #-}

instance PFromJSRef a => PFromJSRef [a] where pfromJSRef = pfromJSRefListOf
                                              {-# INLINE pfromJSRef #-}
instance PFromJSRef Text   where pfromJSRef = pfromJSRef_fromJSString
                                 {-# INLINE pfromJSRef #-}
instance PFromJSRef Char   where
  pfromJSRef x     = C# (jsrefToChar x)
  {-# INLINE pfromJSRef #-}
  pfromJSRefListOf = pfromJSRef_fromJSString
  {-# INLINE pfromJSRefListOf #-}

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

{-
instance (PFromJSRef a, PFromJSRef b) => PFromJSRef (a,b) where
   fromJSRef r = runMaybeT $ (,) <$> jf r 0 <*> jf r 1
   {-# INLINE pfromJSRef #-}
instance (PFromJSRef a, PFromJSRef b, PFromJSRef c) => PFromJSRef (a,b,c) where
   fromJSRef r = runMaybeT $ (,,) <$> jf r 0 <*> jf r 1 <*> jf r 2
   {-# INLINE pfromJSRef #-}
instance (PFromJSRef a, PFromJSRef b, PFromJSRef c, PFromJSRef d) => PFromJSRef (a,b,c,d) where
   fromJSRef r = runMaybeT $ (,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3
   {-# INLINE pfromJSRef #-}
-}

instance PToJSRef (JSRef a) where ptoJSRef = castRef
                                  {-# INLINE ptoJSRef #-}

instance PToJSRef Text where ptoJSRef = ptoJSRef_toJSString
                             {-# INLINE ptoJSRef #-}
instance PToJSRef Char   where
  ptoJSRef (C# c) = charToJSRef c
  {-# INLINE ptoJSRef #-}
  ptoJSRefListOf = ptoJSRef_toJSString
  {-# INLINE ptoJSRefListOf #-}

instance PToJSRef Bool   where ptoJSRef True  = castRef jsTrue
                               ptoJSRef False = castRef jsFalse
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

instance PToJSRef a => PToJSRef [a] where ptoJSRef = ptoJSRefListOf
                                          {-# INLINE ptoJSRef #-}

instance PToJSRef a => PToJSRef (Maybe a) where
    ptoJSRef Nothing  = jsNull
    ptoJSRef (Just a) = castRef (ptoJSRef a)
    {-# INLINE ptoJSRef #-}

instance (PToJSRef a, PToJSRef b) => PToJSRef (a,b) where
  ptoJSRef (a,b) = arr2 (jr a) (jr b)
  {-# INLINE ptoJSRef #-}
instance (PToJSRef a, PToJSRef b, PToJSRef c) => PToJSRef (a,b,c) where
  ptoJSRef (a,b,c) = arr3 (jr a) (jr b) (jr c)
  {-# INLINE ptoJSRef #-}
instance (PToJSRef a, PToJSRef b, PToJSRef c, PToJSRef d) => PToJSRef (a,b,c,d) where
  ptoJSRef (a,b,c,d) = arr4 (jr a) (jr b) (jr c) (jr d)
  {-# INLINE ptoJSRef #-}
instance (PToJSRef a, PToJSRef b, PToJSRef c, PToJSRef d, PToJSRef e) => PToJSRef (a,b,c,d,e) where
  ptoJSRef (a,b,c,d,e) = arr5 (jr a) (jr b) (jr c) (jr d) (jr e)
  {-# INLINE ptoJSRef #-}
instance (PToJSRef a, PToJSRef b, PToJSRef c, PToJSRef d, PToJSRef e, PToJSRef f) => PToJSRef (a,b,c,d,e,f) where
  ptoJSRef (a,b,c,d,e,f) = arr6 (jr a) (jr b) (jr c) (jr d) (jr e) (jr f)
  {-# INLINE ptoJSRef #-}
instance (PToJSRef a, PToJSRef b, PToJSRef c, PToJSRef d, PToJSRef e, PToJSRef f, PToJSRef g) => PToJSRef (a,b,c,d,e,f,g) where
  ptoJSRef (a,b,c,d,e,f,g) = arr7 (jr a) (jr b) (jr c) (jr d) (jr e) (jr f) (jr g)
  {-# INLINE ptoJSRef #-}

jr :: PToJSRef a => a -> JSRef ()
jr = castRef . ptoJSRef
{-# INLINE jr #-}

ptoJSRef_toJSString :: ToJSString a => a -> (JSRef a)
ptoJSRef_toJSString = castRef . toJSString
{-# INLINE ptoJSRef_toJSString #-}

pfromJSRef_fromJSString :: FromJSString a => JSRef a -> a
pfromJSRef_fromJSString = fromJSString . castRef
{-# INLINE pfromJSRef_fromJSString #-}

foreign import javascript unsafe "$r = $1|0;" jsrefToWord   :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1&0xff;" jsrefToWord8  :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1&0xffff;" jsrefToWord16 :: JSRef a -> Word#
foreign import javascript unsafe "$r = $1|0;" jsrefToInt    :: JSRef a -> Int#
foreign import javascript unsafe "$r = $1&0xff;" jsrefToInt8    :: JSRef a -> Int#
foreign import javascript unsafe "$r = $1&0xffff;" jsrefToInt16    :: JSRef a -> Int#
foreign import javascript unsafe "$r = +$1;" jsrefToFloat  :: JSRef a -> Float#
foreign import javascript unsafe "$r = +$1;"  jsrefToDouble :: JSRef a -> Double#
foreign import javascript unsafe "$r = $1&0x7fffffff;" jsrefToChar :: JSRef a -> Char#

foreign import javascript unsafe "$r = $1;" wordToJSRef   :: Word#   -> JSRef a
foreign import javascript unsafe "$r = $1;" intToJSRef    :: Int#    -> JSRef a
foreign import javascript unsafe "$r = $1;" doubleToJSRef :: Double# -> JSRef a
foreign import javascript unsafe "$r = $1;" floatToJSRef  :: Float#  -> JSRef a
foreign import javascript unsafe "$r = $1;" charToJSRef   :: Char#   -> JSRef a

foreign import javascript unsafe "[$1]"                   arr1     :: JSRef a -> JSRef b
foreign import javascript unsafe "[$1,$2]"                arr2     :: JSRef a -> JSRef b -> JSRef c
foreign import javascript unsafe "[$1,$2,$3]"             arr3     :: JSRef a -> JSRef b -> JSRef c -> JSRef d
foreign import javascript unsafe "[$1,$2,$3,$4]"          arr4     :: JSRef a -> JSRef b -> JSRef c -> JSRef d -> JSRef e
foreign import javascript unsafe "[$1,$2,$3,$4,$5]"       arr5     :: JSRef a -> JSRef b -> JSRef c -> JSRef d -> JSRef e -> JSRef f
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6]"    arr6     :: JSRef a -> JSRef b -> JSRef c -> JSRef d -> JSRef e -> JSRef f -> JSRef g
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6,$7]" arr7     :: JSRef a -> JSRef b -> JSRef c -> JSRef d -> JSRef e -> JSRef f -> JSRef g -> JSRef h


