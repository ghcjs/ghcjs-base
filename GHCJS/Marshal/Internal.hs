{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, DefaultSignatures,
             TypeOperators, TupleSections, FlexibleContexts, FlexibleInstances
  #-}

module GHCJS.Marshal.Internal ( FromJSRef(..)
                              , ToJSRef(..)
                              , PToJSRef(..)
                              , PFromJSRef(..)
                              , Purity(..)
                              , toJSRef_generic
                              , fromJSRef_generic
                              , toJSRef_pure
                              , fromJSRef_pure
                              , fromJSRefUnchecked_pure
                              ) where

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Maybe
import           Data.Typeable

import           GHC.Generics

import qualified GHCJS.Prim                 as Prim
import qualified GHCJS.Foreign              as F
import           GHCJS.Types

import qualified Data.JSString.Internal.Type as JSS

import           JavaScript.Array           (MutableJSArray)
import qualified JavaScript.Array.Internal  as AI
import           JavaScript.Object          (Object)
import qualified JavaScript.Object.Internal as OI

data Purity = PureShared    -- ^ conversion is pure even if the original value is shared
            | PureExclusive -- ^ conversion is pure if the we only convert once
  deriving (Eq, Ord, Typeable, Data)

class PToJSRef a where
--  type PureOut a :: Purity
  pToJSRef :: a -> JSRef

class PFromJSRef a where
--  type PureIn a :: Purity
  pFromJSRef :: JSRef -> a

class ToJSRef a where
  toJSRef :: a -> IO JSRef

  toJSRefListOf :: [a] -> IO JSRef
  toJSRefListOf = Prim.toJSArray <=< mapM toJSRef

  -- default toJSRef :: PToJSRef a => a -> IO (JSRef a)
  -- toJSRef x = return (pToJSRef x)

  default toJSRef :: (Generic a, GToJSRef (Rep a ())) => a -> IO JSRef
  toJSRef = toJSRef_generic id

class FromJSRef a where
  fromJSRef :: JSRef -> IO (Maybe a)

  fromJSRefUnchecked :: JSRef -> IO a
  fromJSRefUnchecked = fmap fromJust . fromJSRef
  {-# INLINE fromJSRefUnchecked #-}

  fromJSRefListOf :: JSRef -> IO (Maybe [a])
  fromJSRefListOf = fmap sequence . (mapM fromJSRef <=< Prim.fromJSArray) -- fixme should check that it's an array

  fromJSRefUncheckedListOf :: JSRef -> IO [a]
  fromJSRefUncheckedListOf = mapM fromJSRefUnchecked <=< Prim.fromJSArray
  
  -- default fromJSRef :: PFromJSRef a => JSRef a -> IO (Maybe a)
  -- fromJSRef x = return (Just (pFromJSRef x))

  default fromJSRef :: (Generic a, GFromJSRef (Rep a ())) => JSRef -> IO (Maybe a)
  fromJSRef = fromJSRef_generic id

  -- default fromJSRefUnchecked :: PFromJSRef a => a -> IO a
  -- fromJSRefUnchecked x = return (pFromJSRef x)

-- -----------------------------------------------------------------------------

class GToJSRef a where
  gToJSRef :: (String -> String) -> Bool -> a -> IO JSRef

class GToJSProp a where
  gToJSProp :: (String -> String) -> JSRef -> a -> IO ()

class GToJSArr a where
  gToJSArr :: (String -> String) -> MutableJSArray -> a -> IO ()

instance (ToJSRef b) => GToJSRef (K1 a b c) where
  gToJSRef _ _ (K1 x) = toJSRef x

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
    obj@(OI.Object obj') <- OI.create
    v   <- gToJSRef f (conIsRecord m) x
    OI.setProp (packJSS . f $ conName m) v obj
    return obj'
  gToJSRef f _ m@(M1 x) = gToJSRef f (conIsRecord m) x

instance (GToJSArr (a p), GToJSArr (b p), GToJSProp (a p), GToJSProp (b p)) => GToJSRef ((a :*: b) p) where
  gToJSRef f True xy = do
    (OI.Object obj') <- OI.create
    gToJSProp f obj' xy
    return obj'
  gToJSRef f False xy = do
    arr@(AI.SomeJSArray arr') <- AI.create
    gToJSArr f arr xy
    return arr'

instance GToJSRef (a p) => GToJSRef (M1 S c a p) where
  gToJSRef f b (M1 x) = gToJSRef f b x

instance (GToJSProp (a p), GToJSProp (b p)) => GToJSProp ((a :*: b) p) where
  gToJSProp f o (x :*: y) = gToJSProp f o x >> gToJSProp f o y

instance (Selector c, GToJSRef (a p)) => GToJSProp (M1 S c a p) where
  gToJSProp f o m@(M1 x) = do
    r <- gToJSRef f False x
    OI.setProp (packJSS . f $ selName m) r (OI.Object o)

instance (GToJSArr (a p), GToJSArr (b p)) => GToJSArr ((a :*: b) p) where
  gToJSArr f a (x :*: y) = gToJSArr f a x >> gToJSArr f a y

instance GToJSRef (a p) => GToJSArr (M1 S c a p) where
  gToJSArr f a (M1 x) = do
    r <- gToJSRef f False x
    AI.push r a

instance GToJSRef (V1 p) where
  gToJSRef _ _ _ = return Prim.jsNull

instance GToJSRef (U1 p) where
  gToJSRef _ _ _ = return F.jsTrue

toJSRef_generic :: forall a . (Generic a, GToJSRef (Rep a ()))
                => (String -> String) -> a -> IO JSRef
toJSRef_generic f x = gToJSRef f False (from x :: Rep a ())

-- -----------------------------------------------------------------------------

class GFromJSRef a where
  gFromJSRef :: (String -> String) -> Bool -> JSRef -> IO (Maybe a)

class GFromJSProp a where
  gFromJSProp :: (String -> String) -> JSRef -> IO (Maybe a)

class GFromJSArr a where
  gFromJSArr :: (String -> String) -> MutableJSArray -> Int -> IO (Maybe (a,Int))

instance FromJSRef b => GFromJSRef (K1 a b c) where
  gFromJSRef _ _ r = fmap K1 <$> fromJSRef r

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
    r' <- OI.getProp (packJSS . f $ conName (undefined :: M1 C c a p)) (OI.Object r)
    if isUndefined r'
      then return Nothing
      else fmap M1 <$> gFromJSRef f (conIsRecord (undefined :: M1 C c a p)) r'
  gFromJSRef f _ r = fmap M1 <$> gFromJSRef f (conIsRecord (undefined :: M1 C c a p)) r

instance (GFromJSArr (a p), GFromJSArr (b p), GFromJSProp (a p), GFromJSProp (b p)) => GFromJSRef ((a :*: b) p) where
  gFromJSRef f True  r = gFromJSProp f r
  gFromJSRef f False r = fmap fst <$> gFromJSArr f (AI.SomeJSArray r) 0

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
    p <- OI.getProp (packJSS . f $ selName (undefined :: M1 S c a p)) (OI.Object o)
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
    r <- AI.read n o
    if isUndefined r
      then return Nothing
      else fmap ((,n+1) . M1) <$> gFromJSRef f False r

instance GFromJSRef (V1 p) where
  gFromJSRef _ _ _ = return Nothing

instance GFromJSRef (U1 p) where
  gFromJSRef _ _ _ = return (Just U1)

fromJSRef_generic :: forall a . (Generic a, GFromJSRef (Rep a ()))
                => (String -> String) -> JSRef -> IO (Maybe a)
fromJSRef_generic f x = fmap to <$> (gFromJSRef f False x :: IO (Maybe (Rep a ())))

-- -----------------------------------------------------------------------------

fromJSRef_pure :: PFromJSRef a => JSRef -> IO (Maybe a)
fromJSRef_pure x = return (Just (pFromJSRef x))
{-# INLINE fromJSRef_pure #-}

fromJSRefUnchecked_pure :: PFromJSRef a => JSRef -> IO a
fromJSRefUnchecked_pure x = return (pFromJSRef x)
{-# INLINE fromJSRefUnchecked_pure #-}

toJSRef_pure :: PToJSRef a => a -> IO JSRef
toJSRef_pure x = return (pToJSRef x)
{-# INLINE toJSRef_pure #-}

-- -----------------------------------------------------------------------------

packJSS :: String -> JSString
packJSS = JSS.JSString . Prim.toJSString
