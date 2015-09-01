{-# LANGUAGE DefaultSignatures,
             TypeOperators,
             ScopedTypeVariables,
             DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
             TupleSections,
             MagicHash,
             CPP,
             JavaScriptFFI,
             ForeignFunctionInterface,
             UnliftedFFITypes,
             BangPatterns
  #-}

module GHCJS.Marshal ( FromJSRef(..)
                     , ToJSRef(..)
                     , toJSRef_aeson
                     , toJSRef_pure
                     ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import qualified Data.Aeson as AE
import           Data.Attoparsec.Number (Number(..))
import           Data.Bits ((.&.))
import           Data.Char (chr, ord)
import qualified Data.HashMap.Strict as H
import           Data.Int (Int8, Int16, Int32)
import qualified Data.JSString as JSS
import qualified Data.JSString.Text as JSS
import           Data.Maybe
import           Data.Scientific (Scientific, scientific, fromFloatDigits)
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Word (Word8, Word16, Word32, Word)
import           Data.Primitive.ByteArray

import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Int
import           GHC.Word
import           GHC.Types
import           GHC.Float
import           GHC.Prim
import           GHC.Generics

import           GHCJS.Types
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal.Pure


import qualified JavaScript.Array           as A
import qualified JavaScript.Array.Internal  as AI
import qualified JavaScript.Object          as O
import qualified JavaScript.Object.Internal as OI

import           GHCJS.Marshal.Internal

instance FromJSRef JSRef where
  fromJSRefUnchecked x = return x
  {-# INLINE fromJSRefUnchecked #-}
  fromJSRef = return . Just
  {-# INLINE fromJSRef #-}
instance FromJSRef () where
  fromJSRefUnchecked = fromJSRefUnchecked_pure
  {-# INLINE fromJSRefUnchecked #-}
  fromJSRef = fromJSRef_pure
--    {-# INLINE fromJSRef #-}
instance FromJSRef a => FromJSRef [a] where
    fromJSRef = fromJSRefListOf
    {-# INLINE fromJSRef #-}
instance FromJSRef a => FromJSRef (Maybe a) where
    fromJSRefUnchecked x | isUndefined x || isNull x = return Nothing
                         | otherwise = fromJSRef x
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef x | isUndefined x || isNull x = return (Just Nothing)
                | otherwise = fmap (fmap Just) fromJSRef x
    {-# INLINE fromJSRef #-}
instance FromJSRef JSString where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Text where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Char where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
    fromJSRefUncheckedListOf = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefListOf #-}
    fromJSRefListOf = fromJSRef_pure
    {-# INLINE fromJSRefUncheckedListOf #-}
instance FromJSRef Bool where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Int where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Int8 where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Int16 where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Int32 where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Word where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Word8 where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Word16 where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Word32 where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Float where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef Double where
    fromJSRefUnchecked = fromJSRefUnchecked_pure
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = fromJSRef_pure
    {-# INLINE fromJSRef #-}
instance FromJSRef AE.Value where
    fromJSRef r = case jsonTypeOf r of
            JSONNull    -> return (Just AE.Null)
            JSONInteger -> liftM (AE.Number . flip scientific 0 . (toInteger :: Int -> Integer))
                 <$> fromJSRef r
            JSONFloat   -> liftM (AE.Number . (fromFloatDigits :: Double -> Scientific))
                 <$> fromJSRef r
            JSONBool    -> liftM AE.Bool  <$> fromJSRef r
            JSONString  -> liftM AE.String <$> fromJSRef r
            JSONArray   -> liftM (AE.Array . V.fromList) <$> fromJSRef r
            JSONObject  -> do
                props <- OI.listProps (OI.Object r)
                runMaybeT $ do
                    propVals <- forM props $ \p -> do
                        v <- MaybeT (fromJSRef =<< OI.getProp p (OI.Object r))
                        return (JSS.textFromJSString p, v)
                    return (AE.Object (H.fromList propVals))
    {-# INLINE fromJSRef #-}
instance (FromJSRef a, FromJSRef b) => FromJSRef (a,b) where
    fromJSRef r = runMaybeT $ (,) <$> jf r 0 <*> jf r 1
    {-# INLINE fromJSRef #-}
instance (FromJSRef a, FromJSRef b, FromJSRef c) => FromJSRef (a,b,c) where
    fromJSRef r = runMaybeT $ (,,) <$> jf r 0 <*> jf r 1 <*> jf r 2
    {-# INLINE fromJSRef #-}
instance (FromJSRef a, FromJSRef b, FromJSRef c, FromJSRef d) => FromJSRef (a,b,c,d) where
    fromJSRef r = runMaybeT $ (,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3
    {-# INLINE fromJSRef #-}
instance (FromJSRef a, FromJSRef b, FromJSRef c, FromJSRef d, FromJSRef e) => FromJSRef (a,b,c,d,e) where
    fromJSRef r = runMaybeT $ (,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4
    {-# INLINE fromJSRef #-}
instance (FromJSRef a, FromJSRef b, FromJSRef c, FromJSRef d, FromJSRef e, FromJSRef f) => FromJSRef (a,b,c,d,e,f) where
    fromJSRef r = runMaybeT $ (,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5
    {-# INLINE fromJSRef #-}
instance (FromJSRef a, FromJSRef b, FromJSRef c, FromJSRef d, FromJSRef e, FromJSRef f, FromJSRef g) => FromJSRef (a,b,c,d,e,f,g) where
    fromJSRef r = runMaybeT $ (,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6
    {-# INLINE fromJSRef #-}
instance (FromJSRef a, FromJSRef b, FromJSRef c, FromJSRef d, FromJSRef e, FromJSRef f, FromJSRef g, FromJSRef h) => FromJSRef (a,b,c,d,e,f,g,h) where
    fromJSRef r = runMaybeT $ (,,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6 <*> jf r 7
    {-# INLINE fromJSRef #-}

jf :: FromJSRef a => JSRef -> Int -> MaybeT IO a
jf r n = MaybeT $ do
  r' <- AI.read n (AI.SomeJSArray r)
  if isUndefined r
    then return Nothing
    else fromJSRef r'

instance ToJSRef JSRef where
  toJSRef = toJSRef_pure
  {-# INLINE toJSRef #-}
instance ToJSRef AE.Value where
    toJSRef = toJSRef_aeson
    {-# INLINE toJSRef #-}
instance ToJSRef JSString where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Text where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Char where
    toJSRef = return . pToJSRef
    {-# INLINE toJSRef #-}
    toJSRefListOf = return . pToJSRef
    {-# INLINE toJSRefListOf #-}
instance ToJSRef Bool where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Int where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Int8 where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Int16 where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Int32 where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Word where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Word8 where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Word16 where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Word32 where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Float where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef Double where
    toJSRef = toJSRef_pure
    {-# INLINE toJSRef #-}
instance ToJSRef a => ToJSRef [a] where
    toJSRef = toJSRefListOf
    {-# INLINE toJSRef #-}
instance ToJSRef a => ToJSRef (Maybe a) where
    toJSRef Nothing  = return jsNull
    toJSRef (Just a) = toJSRef a
    {-# INLINE toJSRef #-}
instance (ToJSRef a, ToJSRef b) => ToJSRef (a,b) where
    toJSRef (a,b) = join $ arr2 <$> toJSRef a <*> toJSRef b
    {-# INLINE toJSRef #-}
instance (ToJSRef a, ToJSRef b, ToJSRef c) => ToJSRef (a,b,c) where
    toJSRef (a,b,c) = join $ arr3 <$> toJSRef a <*> toJSRef b <*> toJSRef c
    {-# INLINE toJSRef #-}
instance (ToJSRef a, ToJSRef b, ToJSRef c, ToJSRef d) => ToJSRef (a,b,c,d) where
    toJSRef (a,b,c,d) = join $ arr4 <$> toJSRef a <*> toJSRef b <*> toJSRef c <*> toJSRef d
    {-# INLINE toJSRef #-}
instance (ToJSRef a, ToJSRef b, ToJSRef c, ToJSRef d, ToJSRef e) => ToJSRef (a,b,c,d,e) where
    toJSRef (a,b,c,d,e) = join $ arr5 <$> toJSRef a <*> toJSRef b <*> toJSRef c <*> toJSRef d <*> toJSRef e
    {-# INLINE toJSRef #-}
instance (ToJSRef a, ToJSRef b, ToJSRef c, ToJSRef d, ToJSRef e, ToJSRef f) => ToJSRef (a,b,c,d,e,f) where
    toJSRef (a,b,c,d,e,f) = join $ arr6 <$> toJSRef a <*> toJSRef b <*> toJSRef c <*> toJSRef d <*> toJSRef e <*> toJSRef f
    {-# INLINE toJSRef #-}
instance (ToJSRef a, ToJSRef b, ToJSRef c, ToJSRef d, ToJSRef e, ToJSRef f, ToJSRef g) => ToJSRef (a,b,c,d,e,f,g) where
    toJSRef (a,b,c,d,e,f,g) = join $ arr7 <$> toJSRef a <*> toJSRef b <*> toJSRef c <*> toJSRef d <*> toJSRef e <*> toJSRef f <*> toJSRef g
    {-# INLINE toJSRef #-}

foreign import javascript unsafe "[$1]"                   arr1     :: JSRef -> IO JSRef
foreign import javascript unsafe "[$1,$2]"                arr2     :: JSRef -> JSRef -> IO JSRef
foreign import javascript unsafe "[$1,$2,$3]"             arr3     :: JSRef -> JSRef -> JSRef -> IO JSRef
foreign import javascript unsafe "[$1,$2,$3,$4]"          arr4     :: JSRef -> JSRef -> JSRef -> JSRef -> IO JSRef
foreign import javascript unsafe "[$1,$2,$3,$4,$5]"       arr5     :: JSRef -> JSRef -> JSRef -> JSRef -> JSRef -> IO JSRef
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6]"    arr6     :: JSRef -> JSRef -> JSRef -> JSRef -> JSRef -> JSRef -> IO JSRef
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6,$7]" arr7     :: JSRef -> JSRef -> JSRef -> JSRef -> JSRef -> JSRef -> JSRef -> IO JSRef

toJSRef_aeson :: AE.ToJSON a => a -> IO JSRef
toJSRef_aeson x = cv (AE.toJSON x)
  where
    cv = convertValue

    convertValue :: AE.Value -> IO JSRef
    convertValue AE.Null       = return jsNull
    convertValue (AE.String t) = return (pToJSRef t)
    convertValue (AE.Array a)  = (\(AI.SomeJSArray x) -> x) <$>
                                 (AI.fromListIO =<< mapM convertValue (V.toList a))
    convertValue (AE.Number n) = toJSRef (realToFrac n :: Double)
    convertValue (AE.Bool b)   = return (toJSBool b)
    convertValue (AE.Object o) = do
      obj@(OI.Object obj') <- OI.create
      mapM_ (\(k,v) -> convertValue v >>= \v' -> OI.setProp (JSS.textToJSString k) v' obj) (H.toList o)
      return obj'


