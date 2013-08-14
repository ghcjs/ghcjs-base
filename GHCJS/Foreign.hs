{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI, 
    CPP, MagicHash, FlexibleInstances, BangPatterns #-}

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
                     , toArray
                     , newArray
                     , fromArray
                     , pushArray
                     , indexArray
                     , lengthArray
                     , newObj
                     , getProp
                     , getPropMaybe
                     , setProp
                     , asyncCallback
                     , asyncCallback1
                     , asyncCallback2
                     , syncCallback
                     , syncCallback1
                     , syncCallback2
                     , freeCallback
                     ) where

import           GHCJS.Types

import           GHC.Prim
import           GHC.Exts

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception (evaluate)
import qualified Data.Text as T
import           Foreign.Ptr
import           Unsafe.Coerce


import qualified Data.Text.Array as A

syncCallback :: Bool -> Bool -> IO a -> IO (JSFun (IO a))
syncCallback retain continueAsync x = do
  x' <- evaluate x
  js_syncCallback retain continueAsync (unsafeCoerce x')

syncCallback1 :: Bool -> Bool -> (JSRef a -> IO b) -> IO (JSFun (JSRef a -> IO b))
syncCallback1 retain continueAsync x = do
  x' <- evaluate x
  js_syncCallbackApply retain continueAsync 1 (unsafeCoerce x')

syncCallback2 :: Bool -> Bool -> (JSRef a -> JSRef b -> IO c) -> IO (JSFun (JSRef a -> JSRef b -> IO c))
syncCallback2 retain continueAsync x = do
  x' <- evaluate x
  js_syncCallbackApply retain continueAsync 2 (unsafeCoerce x')

asyncCallback :: Bool -> IO a -> IO (JSFun (IO a))
asyncCallback retain x = do
  x' <- evaluate x
  js_asyncCallback retain (unsafeCoerce x')

asyncCallback1 :: Bool -> (JSRef a -> IO b) -> IO (JSFun (JSRef a -> IO b))
asyncCallback1 retain x = do
  x' <- evaluate x
  js_asyncCallbackApply retain 1 (unsafeCoerce x')

asyncCallback2 :: Bool -> (JSRef a -> JSRef b -> IO c) -> IO (JSFun (JSRef a -> JSRef b -> IO c))
asyncCallback2 retain x = do
  x' <- evaluate x
  js_asyncCallbackApply retain 2 (unsafeCoerce x')

freeCallback :: JSFun a -> IO ()
freeCallback = js_freeCallback

foreign import javascript unsafe "$r = h$toStr($1,$2,$3);" js_toString :: Ref# -> Int# -> Int# -> Ref#
foreign import javascript unsafe "$r = h$fromStr($1); $r2 = h$ret1;" js_fromString :: Ref# -> Ptr ()
foreign import javascript unsafe "$r = $1;" js_fromBool :: JSBool -> Bool
foreign import javascript unsafe "$1 ? true : false" js_isTruthy :: JSRef a -> Bool
foreign import javascript unsafe "$r = true"  js_true :: Int# -> Ref#
foreign import javascript unsafe "$r = false" js_false :: Int# -> Ref#
foreign import javascript unsafe "$r = null"  js_null :: Int# -> Ref#
foreign import javascript unsafe "$r = undefined"  js_undefined :: Int# -> Ref#
foreign import javascript unsafe "$r = []" js_emptyArray :: IO (JSArray a)
foreign import javascript unsafe "$r = {}" js_emptyObj :: IO (JSRef a)
foreign import javascript unsafe "$2.push($1)" js_push :: JSRef a -> JSArray a -> IO ()
foreign import javascript unsafe "$1.length" js_length :: JSArray a -> IO Int
foreign import javascript unsafe "$2[$1]" js_index :: Int -> JSArray a -> IO (JSRef a)
foreign import javascript unsafe "$2[$1]" js_getProp :: JSString -> JSRef a -> IO (JSRef b)
foreign import javascript unsafe "$3[$1] = $2" js_setProp :: JSString -> JSRef a -> JSRef b -> IO ()

foreign import javascript unsafe "h$makeCallback($1, h$runSync, [$2], $3)"
  js_syncCallback :: Bool -> Bool -> Int -> IO (JSFun (IO a))
foreign import javascript unsafe "h$makeCallback($1, h$run, [], $2)"
  js_asyncCallback :: Bool -> Int -> IO (JSFun (IO a))

foreign import javascript unsafe "h$makeCallbackApply($1, $3, h$runSync, [$2], $4)"
  js_syncCallbackApply :: Bool -> Bool -> Int -> Int -> IO (JSRef a)
foreign import javascript unsafe "h$makeCallbackApply($1, $2, h$run, [], $3)"
  js_asyncCallbackApply :: Bool -> Int -> Int -> IO (JSRef a)

foreign import javascript unsafe "h$freeCallback($1)"
  js_freeCallback :: JSFun a -> IO ()

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
  fromJSString (JSRef ref) =
    let !(Ptr' ba l) = ptrToPtr' (js_fromString ref)
    in  unsafeCoerce (Text' (Array' ba) 0 (I# l))
  {-# INLINE fromJSString #-}

instance ToJSString JSString where
  toJSString t = t
  {-# INLINE toJSString #-}

instance FromJSString JSString where
  fromJSString t = t
  {-# INLINE fromJSString #-}

instance IsString JSString where
  fromString = toJSString
  {-# INLINE fromString #-}

fromJSBool :: JSBool -> Bool
fromJSBool b = js_fromBool b
{-# INLINE fromJSBool #-}

toJSBool :: Bool -> JSBool
toJSBool True = jsTrue
toJSBool _    = jsFalse
{-# INLINE toJSBool #-}

-- check whether a reference is `truthy' in the JavaScript sense
fromJSBool' :: JSRef a -> Bool
fromJSBool' b = js_isTruthy b
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

toArray :: [JSRef a] -> IO (JSArray a)
toArray xs = do
  a <- js_emptyArray
  let go ys = case ys of
                (y:ys') -> js_push y a >> go ys'
                _       -> return ()
  go xs
  return a
{-# INLINE toArray #-}

pushArray :: JSRef b -> JSArray a -> IO ()
pushArray r arr = js_push (castRef r) arr
{-# INLINE pushArray #-}

fromArray :: JSArray a -> IO [JSRef a]
fromArray a = do
  l <- js_length a
  let go i | i < l     = (:) <$> js_index i a <*> go (i+1)
           | otherwise = return []
  go 0
{-# INLINE fromArray #-}

lengthArray :: JSArray a -> IO Int
lengthArray a = js_length a
{-# INLINE lengthArray #-}

indexArray :: Int -> JSArray a -> IO (JSRef a)
indexArray = js_index
{-# INLINE indexArray #-}

newArray :: IO (JSArray a)
newArray = js_emptyArray
{-# INLINE newArray #-}

newObj :: IO (JSRef a)
newObj = js_emptyObj
{-# INLINE newObj #-}

getProp :: ToJSString a => a -> JSRef b -> IO (JSRef c)
getProp p o = js_getProp (toJSString p) o
{-# INLINE getProp #-}

getPropMaybe :: ToJSString a => a -> JSRef b -> IO (Maybe (JSRef c))
getPropMaybe p o = do
  p' <- js_getProp (toJSString p) o
  if isUndefined p' then return Nothing else return (Just p')
{-# INLINE getPropMaybe #-}

setProp :: ToJSString a => a -> JSRef b -> JSRef c -> IO ()
setProp p v o = js_setProp (toJSString p) v o
{-# INLINE setProp #-}
