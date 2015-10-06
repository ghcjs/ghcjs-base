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
             BangPatterns,
             TypeFamilies,
             StandaloneDeriving,
             GeneralizedNewtypeDeriving
  #-}

-- | This module provides functions for marshaling Haskell datatypes to
-- and from javascript references. The typesafe marshaling API should be
-- used for most things ('fromJS', 'toJS', etc), as it helps enforce
-- that the datatype matches up with the reference type.
--
-- = Laws
--
-- 'fromJS' should convert back to the value given to 'toJS':
--
-- > (toJS >=> fromJS) === (return . Just)
--
-- If 'fromJS' successfully converts some ref, then 'toJS' should yield
-- a ref which is semantically equivalent (though not necessarily the
-- same ref).
--
-- > (\x -> fromJS >>= maybe (return x) toJS) === return
--
-- (With @(===)@ indicating some reasonable notion of equivalence)
--
-- = Builtin instances
--
-- This module defines a variety of marshaling instances. Here's how
-- they work:
--
-- * All of the numeric types marshal to and from 'JSNumber'.
--
-- * 'String' gets marshaled to 'JSString', whereas all other lists get
-- marshaled to 'AI.JSArray'. This is accomplished with some type system
-- hackery - see "GHCJS.Marshal.List" for details.
--
-- * @('Maybe' a)@ marshals to and from @('Nullable' ('JSType' a))@.
-- 'Nothing' becomes 'jsNull', whereas 'Just' uses the marshaling for
-- @a@. Some type system hackery is used to ensure that we don't try to
-- marshal nested maybes, as @Nothing@ can't be distinguished from @Just
-- Nothing@. See "GHCJS.Marshal.Maybe" for details.
--
-- * Tuples up to arity 7 have instances for marshaling to and from
-- 'JSArray'.
--
-- * Reference types get marshaled by simply passing them through. This
-- needs to be explicitly declared for each reference type, though.
--
-- = Defining new reference types
--
-- New reference types are created by newtype wrapping 'JSVal', like
-- this:
--
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- >
-- > import Data.Typeable (Typeable)
-- > import GHCJS.Marshal
-- > import GHCJS.Marshal.Pure
-- > import GHCJS.Types
-- >
-- > newtype Wrapper = Wrapper JSVal
-- >   deriving (Typeable, IsJSVal, ToJSVal, FromJSVal, PToJSVal, PFromJSVal)
-- > type instance JSType Wrapper = Wrapper
module GHCJS.Marshal
    (
    -- * Typesafe marshaling API
      fromJS
    , fromJSUnchecked
    , pFromJS
    , toJS
    , pToJS
    , JSType
    -- * Typeclasses for marshaling JSVal
    , FromJSVal(..)
    , ToJSVal(..)
    , toJSVal_aeson
    , toJSVal_pure
    ) where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Control.Monad
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import qualified Data.Aeson as AE
import           Data.Bits ((.&.))
import           Data.Char (chr, ord)
import qualified Data.HashMap.Strict as H
import           Data.Int (Int8, Int16, Int32)
import qualified Data.JSString as JSS
import qualified Data.JSString.Text as JSS
import           Data.JSString.Internal.Type (JSString(..))
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

import           GHCJS.Foreign.Internal
import           GHCJS.Internal.Types
import           GHCJS.Marshal.List
import           GHCJS.Marshal.Maybe
import           GHCJS.Marshal.Pure
import           GHCJS.Nullable
import           GHCJS.Types

import qualified JavaScript.Array           as A
import qualified JavaScript.Array.Internal  as AI
import           JavaScript.Boolean
import           JavaScript.Number
import qualified JavaScript.Object          as O
import qualified JavaScript.Object.Internal as OI

import           GHCJS.Marshal.Internal

fromJS :: (FromJSVal a, IsJSVal (JSType a)) => JSType a -> IO (Maybe a)
fromJS = fromJSVal . jsval

fromJSUnchecked :: (FromJSVal a, IsJSVal (JSType a)) => JSType a -> IO a
fromJSUnchecked = fromJSValUnchecked . jsval

pFromJS :: (PFromJSVal a, IsJSVal (JSType a)) => JSType a -> a
pFromJS = pFromJSVal . jsval

toJS :: (ToJSVal a, IsJSVal (JSType a)) => a -> IO (JSType a)
toJS = fmap uncheckedWrapJSVal . toJSVal

pToJS :: (PToJSVal a, IsJSVal (JSType a)) => a -> JSType a
pToJS = uncheckedWrapJSVal . pToJSVal

type family JSType a

type instance JSType JSVal = JSVal
type instance JSType () = JSVal
type instance JSType [a] = JSListType a
type instance JSType (Maybe a) = Nullable (JSType a)
type instance JSType Text = JSString
type instance JSType Char = JSString --FIXME: is this right? Should we have JSChar ?
type instance JSType Bool = Boolean
-- FIXME: consider whether we should have fine grained types for numbers
type instance JSType Int = Number
type instance JSType Int8 = Number
type instance JSType Int16 = Number
type instance JSType Int32 = Number
type instance JSType Word = Number
type instance JSType Word8 = Number
type instance JSType Word16 = Number
type instance JSType Word32 = Number
type instance JSType Float = Number
type instance JSType Double = Number
type instance JSType AE.Value = JSVal
type instance JSType (a, b) = AI.JSArray
type instance JSType (a, b, c) = AI.JSArray
type instance JSType (a, b, c, d) = AI.JSArray
type instance JSType (a, b, c, d, e) = AI.JSArray
type instance JSType (a, b, c, d, e, f) = AI.JSArray
type instance JSType (a, b, c, d, e, f, g) = AI.JSArray

-- -----------------------------------------------------------------------------

-- Define instances for all of the JSVal wrappers which are imported
-- into this module.

type instance JSType JSString = JSString
deriving instance FromJSVal  JSString
deriving instance ToJSVal    JSString
deriving instance PFromJSVal JSString
deriving instance PToJSVal   JSString

type instance JSType (Nullable a) = Nullable a
deriving instance FromJSVal  (Nullable a)
deriving instance ToJSVal    (Nullable a)
deriving instance PFromJSVal (Nullable a)
deriving instance PToJSVal   (Nullable a)

type instance JSType (AI.SomeJSArray m) = AI.SomeJSArray m
deriving instance FromJSVal  (AI.SomeJSArray a)
deriving instance ToJSVal    (AI.SomeJSArray a)
deriving instance PFromJSVal (AI.SomeJSArray a)
deriving instance PToJSVal   (AI.SomeJSArray a)

type instance JSType Boolean = Boolean
deriving instance FromJSVal  Boolean
deriving instance ToJSVal    Boolean
deriving instance PFromJSVal Boolean
deriving instance PToJSVal   Boolean

type instance JSType Number = Number
deriving instance FromJSVal  Number
deriving instance ToJSVal    Number
deriving instance PFromJSVal Number
deriving instance PToJSVal   Number

type instance JSType OI.Object = OI.Object
deriving instance FromJSVal  OI.Object
deriving instance ToJSVal    OI.Object
deriving instance PFromJSVal OI.Object
deriving instance PToJSVal   OI.Object

-- -----------------------------------------------------------------------------

instance FromJSVal JSVal where
  fromJSValUnchecked x = return x
  {-# INLINE fromJSValUnchecked #-}
  fromJSVal = return . Just
  {-# INLINE fromJSVal #-}
instance FromJSVal () where
  fromJSValUnchecked = fromJSValUnchecked_pure
  {-# INLINE fromJSValUnchecked #-}
  fromJSVal = fromJSVal_pure
--    {-# INLINE fromJSVal #-}
instance FromJSVal Text where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Char where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Bool where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Int where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Int8 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Int16 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Int32 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Word where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Word8 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Word16 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Word32 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Float where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Double where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal AE.Value where
    fromJSVal r = case jsonTypeOf r of
            JSONNull    -> return (Just AE.Null)
            JSONInteger -> liftM (AE.Number . flip scientific 0 . (toInteger :: Int -> Integer))
                 <$> fromJSVal r
            JSONFloat   -> liftM (AE.Number . (fromFloatDigits :: Double -> Scientific))
                 <$> fromJSVal r
            JSONBool    -> liftM AE.Bool  <$> fromJSVal r
            JSONString  -> liftM AE.String <$> fromJSVal r
            JSONArray   -> liftM (AE.Array . V.fromList) <$> fromJSVal r
            JSONObject  -> do
                props <- OI.listProps (OI.Object r)
                runMaybeT $ do
                    propVals <- forM props $ \p -> do
                        v <- MaybeT (fromJSVal =<< OI.getProp p (OI.Object r))
                        return (JSS.textFromJSString p, v)
                    return (AE.Object (H.fromList propVals))
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b) => FromJSVal (a,b) where
    fromJSVal r = runMaybeT $ (,) <$> jf r 0 <*> jf r 1
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c) => FromJSVal (a,b,c) where
    fromJSVal r = runMaybeT $ (,,) <$> jf r 0 <*> jf r 1 <*> jf r 2
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d) => FromJSVal (a,b,c,d) where
    fromJSVal r = runMaybeT $ (,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e) => FromJSVal (a,b,c,d,e) where
    fromJSVal r = runMaybeT $ (,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f) => FromJSVal (a,b,c,d,e,f) where
    fromJSVal r = runMaybeT $ (,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f, FromJSVal g) => FromJSVal (a,b,c,d,e,f,g) where
    fromJSVal r = runMaybeT $ (,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f, FromJSVal g, FromJSVal h) => FromJSVal (a,b,c,d,e,f,g,h) where
    fromJSVal r = runMaybeT $ (,,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6 <*> jf r 7
    {-# INLINE fromJSVal #-}

jf :: FromJSVal a => JSVal -> Int -> MaybeT IO a
jf r n = MaybeT $ do
  r' <- AI.read n (AI.SomeJSArray r)
  if isUndefined r
    then return Nothing
    else fromJSVal r'

instance ToJSVal JSVal where
  toJSVal = toJSVal_pure
  {-# INLINE toJSVal #-}
instance ToJSVal AE.Value where
    toJSVal = toJSVal_aeson
    {-# INLINE toJSVal #-}
instance ToJSVal Text where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Char where
    toJSVal = return . pToJSVal
    {-# INLINE toJSVal #-}
instance ToJSVal Bool where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Int where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Int8 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Int16 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Int32 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Word where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Word8 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Word16 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Word32 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Float where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Double where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b) => ToJSVal (a,b) where
    toJSVal (a,b) = join $ arr2 <$> toJSVal a <*> toJSVal b
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a,b,c) where
    toJSVal (a,b,c) = join $ arr3 <$> toJSVal a <*> toJSVal b <*> toJSVal c
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d) => ToJSVal (a,b,c,d) where
    toJSVal (a,b,c,d) = join $ arr4 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e) => ToJSVal (a,b,c,d,e) where
    toJSVal (a,b,c,d,e) = join $ arr5 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f) => ToJSVal (a,b,c,d,e,f) where
    toJSVal (a,b,c,d,e,f) = join $ arr6 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e <*> toJSVal f
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f, ToJSVal g) => ToJSVal (a,b,c,d,e,f,g) where
    toJSVal (a,b,c,d,e,f,g) = join $ arr7 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e <*> toJSVal f <*> toJSVal g
    {-# INLINE toJSVal #-}

foreign import javascript unsafe "[$1]"                   arr1     :: JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2]"                arr2     :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3]"             arr3     :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3,$4]"          arr4     :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3,$4,$5]"       arr5     :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6]"    arr6     :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6,$7]" arr7     :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

toJSVal_aeson :: AE.ToJSON a => a -> IO JSVal
toJSVal_aeson x = cv (AE.toJSON x)
  where
    cv = convertValue

    convertValue :: AE.Value -> IO JSVal
    convertValue AE.Null       = return jsNull
    convertValue (AE.String t) = return (pToJSVal t)
    convertValue (AE.Array a)  = (\(AI.SomeJSArray x) -> x) <$>
                                 (AI.fromListIO =<< mapM convertValue (V.toList a))
    convertValue (AE.Number n) = toJSVal (realToFrac n :: Double)
    convertValue (AE.Bool b)   = return (toJSBool b)
    convertValue (AE.Object o) = do
      obj@(OI.Object obj') <- OI.create
      mapM_ (\(k,v) -> convertValue v >>= \v' -> OI.setProp (JSS.textToJSString k) v' obj) (H.toList o)
      return obj'
