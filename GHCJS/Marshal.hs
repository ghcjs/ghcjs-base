{-# LANGUAGE DefaultSignatures, TypeOperators, ScopedTypeVariables, DefaultSignatures, FlexibleContexts, FlexibleInstances #-}

module GHCJS.Marshal ( FromJSRef(..)
                     , ToJSRef(..)
                     , toJSRef_aeson
                     , toJSRef_generic
                     , toJSRef_toJSString
                     ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import           Data.Attoparsec.Number (Number(..))
import           Data.Bits ((.&.))
import qualified Data.HashMap.Strict as H
import           Data.Int (Int8,Int16,Int32)
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Word (Word8,Word16,Word32,Word)
import           Unsafe.Coerce (unsafeCoerce)

import           GHCJS.Types
import           GHCJS.Foreign

import           GHC.Generics

class ToJSRef a where
  toJSRef :: a -> IO (JSRef a)

--  default toJSRef :: ToJSString a => a -> IO (JSRef a)
--  toJSRef = toJSString

  default toJSRef :: (Generic a, GToJSRef (Rep a ())) => a -> IO (JSRef a)
  toJSRef = toJSRef_generic id

--  default toJSRef :: FromJSON a => a -> IO (JSRef a)
--  toJSRef = toJSRef_aeson

class FromJSRef a where
  fromJSRef :: (JSRef a) -> IO a

  default fromJSRef :: FromJSString a => JSRef a -> IO a
  fromJSRef = return . fromJSString . castRef

--  default fromJSRef :: FromJSON a => JSRef a -> IO a
--  fromJSRef = fromJSRef_aeson

instance FromJSRef (JSRef a) where fromJSRef = return . castRef

instance FromJSRef Text
-- instance FromJSRef String
-- instance FromJSRef a => FromJSRef [a] where fromJSRef = mapM fromJSRef <=< fromArray . castRef
instance FromJSRef Bool   where fromJSRef = return . fromJSBool . castRef
instance FromJSRef Int    where fromJSRef = return . (.&.0xffffffff) . fromJSNumber
instance FromJSRef Int8   where fromJSRef = return . (.&.0xff)       . fromJSNumber
instance FromJSRef Int16  where fromJSRef = return . (.&.0xffff)     . fromJSNumber
instance FromJSRef Int32  where fromJSRef = return . (.&.0xffffffff) . fromJSNumber
instance FromJSRef Word   where fromJSRef = return . (.&.0xffffffff) . fromJSNumber
instance FromJSRef Word8  where fromJSRef = return . (.&.0xff)       . fromJSNumber
instance FromJSRef Word16 where fromJSRef = return . (.&.0xffff)     . fromJSNumber
instance FromJSRef Word32 where fromJSRef = return . (.&.0xffffffff) . fromJSNumber
instance FromJSRef Float  where fromJSRef = return . fromJSNumber
instance FromJSRef Double where fromJSRef = return . fromJSNumber

instance ToJSRef (JSRef a) where toJSRef = fmap castRef . return

instance ToJSRef Text where toJSRef = toJSRef_toJSString
-- instance ToJSRef String
-- instance ToJSRef a => ToJSRef [a] where toJSRef = fmap castRef . (toArray <=< mapM toJSRef)

instance ToJSRef Bool   where toJSRef = return . castRef . toJSBool
instance ToJSRef Int    where toJSRef = return . toJSNumber
instance ToJSRef Int8   where toJSRef = return . toJSNumber
instance ToJSRef Int16  where toJSRef = return . toJSNumber
instance ToJSRef Int32  where toJSRef = return . toJSNumber
instance ToJSRef Word   where toJSRef = return . toJSNumber
instance ToJSRef Word8  where toJSRef = return . toJSNumber
instance ToJSRef Word16 where toJSRef = return . toJSNumber
instance ToJSRef Word32 where toJSRef = return . toJSNumber
instance ToJSRef Float  where toJSRef = return . toJSNumber
instance ToJSRef Double where toJSRef = return . toJSNumber

instance (ToJSRef a, ToJSRef b) => ToJSRef (a,b) where 
  toJSRef (a,b) = ja [jr a, jr b]
instance (ToJSRef a, ToJSRef b, ToJSRef c) => ToJSRef (a,b,c) where 
  toJSRef (a,b,c) = ja [jr a, jr b, jr c]
instance (ToJSRef a, ToJSRef b, ToJSRef c, ToJSRef d) => ToJSRef (a,b,c,d) where 
  toJSRef (a,b,c,d) = ja [jr a, jr b, jr c, jr d]
instance (ToJSRef a, ToJSRef b, ToJSRef c, ToJSRef d, ToJSRef e) => ToJSRef (a,b,c,d,e) where 
  toJSRef (a,b,c,d,e) = ja [jr a, jr b, jr c, jr d, jr e]
instance (ToJSRef a, ToJSRef b, ToJSRef c, ToJSRef d, ToJSRef e, ToJSRef f) => ToJSRef (a,b,c,d,e,f) where 
  toJSRef (a,b,c,d,e,f) = ja [jr a, jr b, jr c, jr d, jr e, jr f]
instance (ToJSRef a, ToJSRef b, ToJSRef c, ToJSRef d, ToJSRef e, ToJSRef f, ToJSRef g) => ToJSRef (a,b,c,d,e,f,g) where 
  toJSRef (a,b,c,d,e,f,g) = ja [jr a, jr b, jr c, jr d, jr e, jr f, jr g]

ja :: [IO (JSRef a)] -> IO (JSRef b)
ja = fmap castRef . (toArray <=< sequence)

jr :: ToJSRef a => a -> IO (JSRef ())
jr a = castRef <$> toJSRef a

fromJSNumber :: JSRef a -> a
fromJSNumber = unsafeCoerce

toJSNumber :: a -> JSRef a
toJSNumber = unsafeCoerce

toJSRef_aeson :: ToJSON a => a -> IO (JSRef a)
toJSRef_aeson x = cv (toJSON x) 
  where
    cv = fmap castRef . convertValue

    convertValue :: Value -> IO (JSRef ())
    convertValue Null       = return jsNull
    convertValue (String t) = return (castRef $ toJSString t)
    convertValue (Array a)  = castRef <$> (toArray =<< mapM convertValue (V.toList a))
    convertValue (Number n) = case n of
                                D d -> return (castRef $ toJSNumber d)
                                I i -> return (castRef . toJSNumber $ realToFrac i)
    convertValue (Bool b)   = return (castRef $ toJSBool b)
    convertValue (Object o) = do
      obj <- newObj
      mapM_ (\(k,v) -> convertValue v >>= \v' -> setProp k v' obj) (H.toList o)
      return obj

toJSRef_toJSString :: ToJSString a => a -> IO (JSRef a)
toJSRef_toJSString = return . castRef . toJSString

class GToJSRef a where
  gToJSRef :: (String -> String) -> Bool -> a -> IO (JSRef ())

class GToJSProp a where
  gToJSProp :: (String -> String) -> JSRef () -> a -> IO ()

class GToJSArr a where
  gToJSArr :: (String -> String) -> JSArray () -> a -> IO ()

instance (ToJSRef b) => GToJSRef (K1 a b c) where
  gToJSRef _ _ (K1 x) = castRef <$> toJSRef x

instance GToJSRef p => GToJSRef (Par1 p) where
  gToJSRef f b (Par1 p) = gToJSRef f b p

instance GToJSRef (f p) => GToJSRef (Rec1 f p) where
  gToJSRef f b (Rec1 x) = gToJSRef f b x

instance (GToJSRef (a p), GToJSRef (b p)) => GToJSRef ((a :+: b) p) where
  gToJSRef f _ (L1 x) = gToJSRef f True x
  gToJSRef f _ (R1 x) = gToJSRef f True x

instance (Datatype c, GToJSRef (a p)) => GToJSRef (M1 D c a p) where
  gToJSRef f b m@(M1 x) = gToJSRef f b x
  
instance (Constructor c, GToJSRef (a p)) => GToJSRef (M1 C c a p) where
  gToJSRef f True m@(M1 x) = do
    obj <- newObj
    v   <- gToJSRef f (conIsRecord m) x
    setProp (f $ conName m) v obj
    return obj
  gToJSRef f _ m@(M1 x) = gToJSRef f (conIsRecord m) x
 
instance (GToJSArr (a p), GToJSArr (b p), GToJSProp (a p), GToJSProp (b p)) => GToJSRef ((a :*: b) p) where
  gToJSRef f True xy = do
    obj <- newObj
    gToJSProp f obj xy
    return (castRef obj)
  gToJSRef f False xy = do
    arr <- newArray
    gToJSArr f arr xy
    return (castRef arr)

instance GToJSRef (a p) => GToJSRef (M1 S c a p) where
  gToJSRef f b (M1 x) = gToJSRef f b x

instance (GToJSProp (a p), GToJSProp (b p)) => GToJSProp ((a :*: b) p) where
  gToJSProp f o (x :*: y) = gToJSProp f o x >> gToJSProp f o y

instance (Selector c, GToJSRef (a p)) => GToJSProp (M1 S c a p) where
  gToJSProp f o m@(M1 x) = do
    r <- gToJSRef f False x
    setProp (f $ selName m) r o

instance (GToJSArr (a p), GToJSArr (b p)) => GToJSArr ((a :*: b) p) where
  gToJSArr f a (x :*: y) = gToJSArr f a x >> gToJSArr f a y

instance (GToJSRef (a p)) => GToJSArr (M1 S c a p) where
  gToJSArr f a (M1 x) = do
    r <- gToJSRef f False x
    pushArray r a

instance GToJSRef (V1 p) where
  gToJSRef _ _ _ = return jsNull

instance GToJSRef (U1 p) where
  gToJSRef _ _ _ = return (castRef jsTrue)

toJSRef_generic :: forall a . (Generic a, GToJSRef (Rep a ()))
                => (String -> String) -> a -> IO (JSRef a)
toJSRef_generic f x = castRef <$> gToJSRef f False (from x :: Rep a ())


