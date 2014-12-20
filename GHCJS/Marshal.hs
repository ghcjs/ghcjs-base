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
                     , toJSRef_generic
                     , fromJSRef_generic
                     ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object)
import           Data.Attoparsec.Number (Number(..))
import           Data.Bits ((.&.))
import           Data.Char (chr, ord)
import qualified Data.HashMap.Strict as H
import           Data.Int (Int8, Int16, Int32)
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

import           GHCJS.Types
import           GHCJS.Foreign
import           GHCJS.Marshal.Pure

import           GHC.Generics

class ToJSRef a where
  toJSRef :: a -> IO (JSRef a)

  toJSRefListOf :: [a] -> IO (JSRef [a])
  toJSRefListOf = fmap castRef . (toArray <=< mapM toJSRef)

  default toJSRef :: (Generic a, GToJSRef (Rep a ())) => a -> IO (JSRef a)
  toJSRef = toJSRef_generic id

class FromJSRef a where
  fromJSRef :: JSRef a -> IO (Maybe a)

  fromJSRefUnchecked :: JSRef a -> IO a
  fromJSRefUnchecked = fmap fromJust . fromJSRef
  {-# INLINE fromJSRefUnchecked #-}

  fromJSRefListOf :: JSRef [a] -> IO (Maybe [a])
  fromJSRefListOf = fmap sequence . (mapM fromJSRef <=< fromArray . castRef) -- fixme should check that it's an array

  fromJSRefUncheckedListOf :: JSRef [a] -> IO [a]
  fromJSRefUncheckedListOf = mapM fromJSRefUnchecked <=< fromArray . castRef

  default fromJSRef :: (Generic a, GFromJSRef (Rep a ())) => JSRef a -> IO (Maybe a)
  fromJSRef = fromJSRef_generic id

instance FromJSRef (JSRef a) where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef () where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef a => FromJSRef [a] where
    fromJSRef = fromJSRefListOf
    {-# INLINE fromJSRef #-}
instance FromJSRef a => FromJSRef (Maybe a) where
    fromJSRef x | isUndefined x || isNull x = return (Just Nothing)
                | otherwise = fmap (fmap Just) fromJSRef (castRef x)
    {-# INLINE fromJSRef #-}
instance FromJSRef Text where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Char where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
    fromJSRefListOf = return . pfromJSRef . castRef
    {-# INLINE fromJSRefListOf #-}
    fromJSRefUncheckedListOf = return . pfromJSRef
    {-# INLINE fromJSRefUncheckedListOf #-}
instance FromJSRef Bool where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Int where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Int8 where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Int16 where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Int32 where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Word where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Word8 where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Word16 where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Word32 where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Float where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Double where
    fromJSRefUnchecked = return . pfromJSRef
    {-# INLINE fromJSRefUnchecked #-}
    fromJSRef = return . pfromJSRef . castRef
    {-# INLINE fromJSRef #-}
instance FromJSRef Value where
    fromJSRef r = do
        ty <- typeOf r
        -- 0 - null, 1 - integer,
        -- 2 - float, 3 - bool,
        -- 4 - string, 5 - array
        -- 6 - object
        case ty of
            0 -> return (Just Null)
            1 -> liftM (Number . flip scientific 0 . (toInteger :: Int -> Integer))
                 <$> (fromJSRef $ castRef r)
            2 -> liftM (Number . (fromFloatDigits :: Double -> Scientific))
                 <$> fromJSRef (castRef r)
            3 -> liftM Bool  <$> fromJSRef (castRef r)
            4 -> liftM String <$> fromJSRef (castRef r)
            5 -> liftM (Array . V.fromList) <$> fromJSRef (castRef r)
            6 -> do
                props <- listProps r
                runMaybeT $ do
                    propVals <- forM props $ \p -> do
                        v <- MaybeT (fromJSRef =<< getProp p r)
                        return (fromJSString p, v)
                    return (Object (H.fromList propVals))
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

jf :: FromJSRef a => JSRef b -> Int -> MaybeT IO a
jf r n = MaybeT $ do
  r' <- indexArray n (castRef r)
  if isUndefined r
    then return Nothing
    else fromJSRef r'

instance ToJSRef (JSRef a) where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Value where
    toJSRef = toJSRef_aeson
    {-# INLINE toJSRef #-}
instance ToJSRef Text where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Char   where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
    toJSRefListOf = return . ptoJSRef
    {-# INLINE toJSRefListOf #-}
instance ToJSRef Bool where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Int where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Int8 where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Int16 where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Int32 where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Word where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Word8 where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Word16 where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Word32 where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Float where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef Double where
    toJSRef = return . ptoJSRef
    {-# INLINE toJSRef #-}
instance ToJSRef a => ToJSRef [a] where
    toJSRef = toJSRefListOf
    {-# INLINE toJSRef #-}
instance ToJSRef a => ToJSRef (Maybe a) where
    toJSRef Nothing  = return jsNull
    toJSRef (Just a) = castRef <$> toJSRef a
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

foreign import javascript unsafe "[$1]"                   arr1     :: JSRef a -> IO (JSRef b)
foreign import javascript unsafe "[$1,$2]"                arr2     :: JSRef a -> JSRef b -> IO (JSRef c)
foreign import javascript unsafe "[$1,$2,$3]"             arr3     :: JSRef a -> JSRef b -> JSRef c -> IO (JSRef d)
foreign import javascript unsafe "[$1,$2,$3,$4]"          arr4     :: JSRef a -> JSRef b -> JSRef c -> JSRef d -> IO (JSRef e)
foreign import javascript unsafe "[$1,$2,$3,$4,$5]"       arr5     :: JSRef a -> JSRef b -> JSRef c -> JSRef d -> JSRef e -> IO (JSRef f)
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6]"    arr6     :: JSRef a -> JSRef b -> JSRef c -> JSRef d -> JSRef e -> JSRef f -> IO (JSRef g)
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6,$7]" arr7     :: JSRef a -> JSRef b -> JSRef c -> JSRef d -> JSRef e -> JSRef f -> JSRef g -> IO (JSRef h)

toJSRef_aeson :: ToJSON a => a -> IO (JSRef a)
toJSRef_aeson x = cv (toJSON x)
  where
    cv = fmap castRef . convertValue

    convertValue :: Value -> IO (JSRef ())
    convertValue Null       = return jsNull
    convertValue (String t) = return (castRef $ toJSString t)
    convertValue (Array a)  = castRef <$> (toArray =<< mapM convertValue (V.toList a))
    convertValue (Number n) = castRef <$> toJSRef (realToFrac n :: Double)
    convertValue (Bool b)   = return (castRef $ toJSBool b)
    convertValue (Object o) = do
      obj <- newObj
      mapM_ (\(k,v) -> convertValue v >>= \v' -> setProp k v' obj) (H.toList o)
      return obj

class GToJSRef a where
  gToJSRef :: (String -> String) -> Bool -> a -> IO (JSRef ())

class GToJSProp a where
  gToJSProp :: (String -> String) -> JSRef () -> a -> IO ()

class GToJSArr a where
  gToJSArr :: (String -> String) -> JSArray (JSRef ()) -> a -> IO ()

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

instance GToJSRef (a p) => GToJSArr (M1 S c a p) where
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

class GFromJSRef a where
  gFromJSRef :: (String -> String) -> Bool -> JSRef () -> IO (Maybe a)

class GFromJSProp a where
  gFromJSProp :: (String -> String) -> JSRef () -> IO (Maybe a)

class GFromJSArr a where
  gFromJSArr :: (String -> String) -> JSArray (JSRef ()) -> Int -> IO (Maybe (a,Int))

instance FromJSRef b => GFromJSRef (K1 a b c) where
  gFromJSRef _ _ r = fmap K1 <$> fromJSRef (castRef r)

instance GFromJSRef p => GFromJSRef (Par1 p) where
  gFromJSRef f b r = gFromJSRef f b r

instance GFromJSRef (f p) => GFromJSRef (Rec1 f p) where
  gFromJSRef f b r = gFromJSRef f b r

instance (GFromJSRef (a p), GFromJSRef (b p)) => GFromJSRef ((a :+: b) p) where
  gFromJSRef f b r = do
    l <- gFromJSRef f True r
    case l of
      Just x  -> return (L1 <$> Just x)
      Nothing -> fmap R1 <$> gFromJSRef f True r

instance (Datatype c, GFromJSRef (a p)) => GFromJSRef (M1 D c a p) where
  gFromJSRef f b r = fmap M1 <$> gFromJSRef f b r

instance forall c a p . (Constructor c, GFromJSRef (a p)) => GFromJSRef (M1 C c a p) where
  gFromJSRef f True r = do
    r' <- getProp (f (conName (undefined :: M1 C c a p))) r
    if isUndefined r'
      then return Nothing
      else fmap M1 <$> gFromJSRef f (conIsRecord (undefined :: M1 C c a p)) r'
  gFromJSRef f _ r = fmap M1 <$> gFromJSRef f (conIsRecord (undefined :: M1 C c a p)) r

instance (GFromJSArr (a p), GFromJSArr (b p), GFromJSProp (a p), GFromJSProp (b p)) => GFromJSRef ((a :*: b) p) where
  gFromJSRef f True  r = gFromJSProp f r
  gFromJSRef f False r = fmap fst <$> gFromJSArr f (castRef r) 0

instance GFromJSRef (a p) => GFromJSRef (M1 S c a p) where
  gFromJSRef f b r = fmap M1 <$> gFromJSRef f b r

instance (GFromJSProp (a p), GFromJSProp (b p)) => GFromJSProp ((a :*: b) p) where
  gFromJSProp f r = do
    a <- gFromJSProp f r
    case a of
      Nothing -> return Nothing
      Just a' -> fmap (a':*:) <$> gFromJSProp f r

instance forall c a p . (Selector c, GFromJSRef (a p)) => GFromJSProp (M1 S c a p) where
  gFromJSProp f o = do
    p <- getProp (f $ selName (undefined :: M1 S c a p)) o
    if isUndefined p
      then return Nothing
      else fmap M1 <$> gFromJSRef f False p

instance (GFromJSArr (a p), GFromJSArr (b p)) => GFromJSArr ((a :*: b) p) where
  gFromJSArr f r n = do
    a <- gFromJSArr f r 0
    case a of
      Just (a',an) -> do
        b <- gFromJSArr f r an
        case b of
          Just (b',bn) -> return (Just (a' :*: b',bn))
          _            -> return Nothing

instance (GFromJSRef (a p)) => GFromJSArr (M1 S c a p) where
  gFromJSArr f o n = do
    r <- indexArray n o
    if isUndefined r
      then return Nothing
      else fmap ((,n+1) . M1) <$> gFromJSRef f False r

instance GFromJSRef (V1 p) where
  gFromJSRef _ _ _ = return Nothing

instance GFromJSRef (U1 p) where
  gFromJSRef _ _ _ = return (Just U1)

fromJSRef_generic :: forall a . (Generic a, GFromJSRef (Rep a ()))
                => (String -> String) -> JSRef a -> IO (Maybe a)
fromJSRef_generic f x = fmap to <$> (gFromJSRef f False (castRef x) :: IO (Maybe (Rep a ())))


