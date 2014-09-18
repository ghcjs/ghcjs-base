{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI,
    MagicHash, FlexibleInstances, BangPatterns, Rank2Types, CPP #-}

{- | Basic interop between Haskell and JavaScript.

     The principal type here is 'JSRef', which is a lifted type that contains
     a JavaScript reference. The 'JSRef' type is parameterized with one phantom
     type, and GHCJS.Types defines several type synonyms for specific variants.

     The code in this module makes no assumptions about 'JSRef a' types.
     Operations that can result in a JS exception that can kill a Haskell thread
     are marked unsafe (for example if the 'JSRef' contains a null or undefined
     value). There are safe variants where the JS exception is propagated as
     a Haskell exception, so that it can be handled on the Haskell side.

     For more specific types, like 'JSArray' or 'JSBool', the code assumes that
     the contents of the 'JSRef' actually is a JavaScript array or bool value.
     If it contains an unexpected value, the code can result in exceptions that
     kill the Haskell thread, even for functions not marked unsafe.

     The code makes use of `foreign import javascript', enabled with the
     `JavaScriptFFI` extension, available since GHC 7.8. There are three different
     safety levels:

      * unsafe: The imported code is run directly. returning an incorrectly typed
        value leads to undefined behaviour. JavaScript exceptions in the foreign
        code kill the Haskell thread.
      * safe: Returned values are replaced with a default value if they have
        the wrong type. JavaScript exceptions are caught and propagated as
        Haskell exceptions ('JSException'), so they can be handled with the
        standard "Control.Exception" machinery.
      * interruptible: The import is asynchronous. The calling Haskell thread
        sleeps until the foreign code calls the `$c` JavaScript function with
        the result. The thread is in interruptible state while blocked, so it
        can receive asynchronous exceptions.

     Unlike the FFI for native code, it's safe to call back into Haskell
     (`h$run`, `h$runSync`) from foreign code in any of the safety levels.
     Since JavaScript is single threaded, no Haskell threads can run while
     the foreign code is running.
 -}

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
                     , getProp, unsafeGetProp
                     , getPropMaybe, unsafeGetPropMaybe
                     , setProp, unsafeSetProp
                     , listProps
                     , typeOf
                     , asyncCallback
                     , asyncCallback1
                     , asyncCallback2
                     , syncCallback
                     , syncCallback1
                     , syncCallback2
                     , ForeignRetention(..)
                     , retain, retainDom
                     , release, releaseDom, releaseAll
                     , wrapBuffer, wrapMutableBuffer
                     , byteArrayJSRef, mutableByteArrayJSRef
                     , bufferByteString, byteArrayByteString
                     , unsafeMutableByteArrayByteString
                     ) where

import           GHCJS.Types
import qualified GHCJS.Prim as Prim

import           GHC.Prim
import           GHC.Exts

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception (evaluate, Exception)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (Text, toStrict, fromStrict)

import           Foreign.ForeignPtr.Safe
import           Foreign.Ptr

import           Data.Primitive.Addr (Addr(..))
import           Data.Primitive.ByteArray

import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import qualified Data.Text.Array as A

import           Unsafe.Coerce


{- |
  Retention options affect how callbacks keep Haskell data alive.
  Haskell threads keep the data they reference alive, as long as they
  are known by the scheduler (running, delayed, blocked on MVar,
  async IO operation etc).

  When you make a callback, a JavaScript function that runs some
  Haskell code in a regular or asynchronous thread, the runtime
  cannot determine whether there are still JS references to the callback.

  When a callback is not retained, the following will happen:

   * Weak references are not kept alive
   * CAFs are reset to their unevaluated state, some recomputation
     may be necessary
   * The action does not keep MVars alive, threads blocked on the
     MVar can get an exception unless there are other references.

  When a callback is retained by the RTS, the function and all the
  data it references is kept alive and cannot be garbage collected.
  Be careful to not create memory leaks.

  You can still use a callback even if it's not retained, as long as
  you bear in mind the caveats mentioned above. Note that lazy IO
  or FRP libraries might depend on weak references internally.
 -}

data ForeignRetention
  = NeverRetain                   -- ^ do not retain data unless the callback is directly
                                  --   referenced by a Haskell thread.
  | AlwaysRetain                  -- ^ retain references indefinitely, until `freeCallback`
                                  --   is called (the callback will be kept in memory until it's freed)
  | DomRetain (forall a. JSRef a) -- ^ retain data as long as the `JSRef` is a DOM element in
                                  --   `window.document` or in a DOM tree referenced by a Haskell
                                  --    thread.

retentionRef :: ForeignRetention -> JSRef ()
retentionRef NeverRetain   = castRef jsFalse
retentionRef AlwaysRetain  = castRef jsTrue
retentionRef (DomRetain r) = castRef r

{- | Make a callback (JavaScript function) that runs the supplied IO action in a synchronous
     thread when called.
 -}
syncCallback :: ForeignRetention                        -- ^ how Haskell data is retained by the callback
             -> Bool                                    -- ^ continue thread asynchronously when blocked, see `GHCJS.Concurrent`
             -> IO a                                    -- ^ the Haskell action
             -> IO (JSFun (IO a))                       -- ^ the callback
syncCallback retain continueAsync x = do
  x' <- evaluate x
  js_syncCallback (retentionRef retain) continueAsync (unsafeCoerce x')


{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes one argument that it passes as a JSRef value to
     the Haskell function
 -}
syncCallback1 :: ForeignRetention                        -- ^ how Haskell data is retained by the callback
              -> Bool                                    -- ^ continue thread asynchronously when blocked, see `GHCJS.Concurrent`
              -> (JSRef a -> IO b)                       -- ^ the Haskell function
              -> IO (JSFun (JSRef a -> IO b))            -- ^ the callback
syncCallback1 retain continueAsync x = do
  x' <- evaluate x
  js_syncCallbackApply (retentionRef retain) continueAsync 1 (unsafeCoerce x')


{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes two arguments that it passes as JSRef values to
     the Haskell function
 -}
syncCallback2 :: ForeignRetention                        -- ^ how Haskell data is retained by the callback
              -> Bool                                    -- ^ continue thread asynchronously when blocked, see `GHCJS.Concurrent`
              -> (JSRef a -> JSRef b -> IO c)            -- ^ the Haskell function
              -> IO (JSFun (JSRef a -> JSRef b -> IO c)) -- ^ the callback
syncCallback2 retain continueAsync x = do
  x' <- evaluate x
  js_syncCallbackApply (retentionRef retain) continueAsync 2 (unsafeCoerce x')


{- | Make a callback (JavaScript function) that runs the supplied IO action in an asynchronous
     thread when called.
 -}
asyncCallback :: ForeignRetention  -- ^ how Haskell data is retained by the callback
              -> IO a              -- ^ the action that the callback runs
              -> IO (JSFun (IO a)) -- ^ the callback
asyncCallback retain x = do
  x' <- evaluate x
  js_asyncCallback (retentionRef retain) (unsafeCoerce x')

asyncCallback1 :: ForeignRetention             -- ^ how Haskell data is retained by the callback
               -> (JSRef a -> IO b)            -- ^ the function that the callback calls
               -> IO (JSFun (JSRef a -> IO b)) -- ^ the calback
asyncCallback1 retain x = do
  x' <- evaluate x
  js_asyncCallbackApply (retentionRef retain) 1 (unsafeCoerce x')

asyncCallback2 :: ForeignRetention                        -- ^ how Haskell data is retained by the callback
               -> (JSRef a -> JSRef b -> IO c)            -- ^ the Haskell function that the callback calls
               -> IO (JSFun (JSRef a -> JSRef b -> IO c)) -- ^ the callback
asyncCallback2 retain x = do
  x' <- evaluate x
  js_asyncCallbackApply (retentionRef retain) 2 (unsafeCoerce x')

{- | Retain the data associated with this callback until `release` is called. -}
retain :: JSFun a -- ^ the callback
       -> IO ()
retain = js_retain
{-# INLINE retain #-}

{- | Retain the data associated with this callback as long as the DOM
     element is in `window.document` or in a DOM tree referenced by
     an active Haskell thread.
 -}
retainDom :: JSRef a -- ^ the DOM element
          -> JSFun b -- ^ the callback
          -> IO ()
retainDom = js_retainDom
{-#  INLINE retainDom #-}

{- | Free all retention associated with the callback
 -}
releaseAll :: JSFun a  -- ^ the callback
        -> IO ()
releaseAll = js_releaseAll
{-# INLINE releaseAll #-}

{- | free the retention associated with DOM element for the callback, if any
 -}
releaseDom :: JSRef a -- ^ the DOM element
           -> JSFun b -- ^ the callback
           -> IO ()
releaseDom = js_releaseDom
{-# INLINE releaseDom #-}

{- | remove any permanent retention associated with the callback, DOM retention
     is unchanged.
 -}
release :: JSFun a -- ^ the callback
                 -> IO ()
release= js_release

foreign import javascript unsafe "$r = h$toStr($1,$2,$3);" js_toString :: Ref# -> Int# -> Int# -> Ref#
foreign import javascript unsafe "$r = h$fromStr($1); $r2 = h$ret1;" js_fromString :: Ref# -> Ptr ()
foreign import javascript unsafe "$r = $1;" js_unsafeFromBool :: JSBool -> Bool
foreign import javascript unsafe "$1 ? true : false" js_isTruthy :: JSRef a -> Bool
foreign import javascript unsafe "$r = true"  js_true :: Int# -> Ref#
foreign import javascript unsafe "$r = false" js_false :: Int# -> Ref#
foreign import javascript unsafe "$r = null"  js_null :: Int# -> Ref#
foreign import javascript unsafe "$r = undefined"  js_undefined :: Int# -> Ref#
foreign import javascript unsafe "$r = []" js_emptyArray :: IO (JSArray a)
foreign import javascript unsafe "$r = {}" js_emptyObj :: IO (JSRef a)
foreign import javascript safe "$2.push($1)" js_push :: JSRef a -> JSArray a -> IO ()
foreign import javascript safe "$1.length" js_length :: JSArray a -> IO Int
foreign import javascript unsafe "$2.push($1)" js_unsafePush :: JSRef a -> JSArray a -> IO ()
foreign import javascript unsafe "$1.length" js_unsafeLength :: JSArray a -> IO Int
foreign import javascript safe "$2[$1]" js_index :: Int -> JSArray a -> IO (JSRef a)
foreign import javascript safe "$2[$1]" js_getProp :: JSString -> JSRef a -> IO (JSRef b)
foreign import javascript safe "$3[$1] = $2" js_setProp :: JSString -> JSRef a -> JSRef b -> IO ()
foreign import javascript unsafe "$2[$1]" js_unsafeIndex :: Int -> JSArray a -> IO (JSRef a)
foreign import javascript unsafe "$2[$1]" js_unsafeGetProp :: JSString -> JSRef a -> IO (JSRef b)
foreign import javascript unsafe "$3[$1] = $2" js_unsafeSetProp :: JSString -> JSRef a -> JSRef b -> IO ()
foreign import javascript safe "h$listprops($1)" js_listProps :: JSRef a -> IO (JSArray JSString)
foreign import javascript unsafe "h$typeOf($1)" js_typeOf :: JSRef a -> IO Int

foreign import javascript unsafe "h$makeCallback($1, h$runSync, [$2], $3)"
  js_syncCallback :: JSRef a -> Bool -> Double -> IO (JSFun (IO b))
foreign import javascript unsafe "h$makeCallback($1, h$run, [], $2)"
  js_asyncCallback :: JSRef a -> Double -> IO (JSFun (IO b))

foreign import javascript unsafe "h$makeCallbackApply($1, $3, h$runSync, [$2], $4)"
  js_syncCallbackApply :: JSRef a -> Bool -> Int -> Double -> IO (JSRef b)
foreign import javascript unsafe "h$makeCallbackApply($1, $2, h$run, [], $3)"
  js_asyncCallbackApply :: JSRef a -> Int -> Double -> IO (JSRef b)

foreign import javascript unsafe "h$retain($1)"
  js_retain :: JSFun a -> IO ()

foreign import javascript unsafe "h$retainDom($1, $2)"
  js_retainDom :: JSRef a -> JSFun b -> IO ()

foreign import javascript unsafe "h$releaseAll($1)"
  js_releaseAll :: JSFun a -> IO ()

foreign import javascript unsafe "h$releaseDom($1)"
  js_releaseDom :: JSRef a -> JSFun b -> IO ()

foreign import javascript unsafe "h$release($1)"
  js_release :: JSFun a -> IO ()

class ToJSString a where
  toJSString :: a -> JSString

class FromJSString a where
  fromJSString :: JSString -> a

instance ToJSString [Char] where
  toJSString = Prim.toJSString
  {-# INLINE toJSString #-}

instance FromJSString [Char] where
  fromJSString = Prim.fromJSString
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

instance ToJSString TL.Text where
  toJSString = toJSString . TL.toStrict
  {-# INLINE toJSString #-}

instance FromJSString TL.Text where
  fromJSString = TL.fromStrict . fromJSString
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
fromJSBool b = js_unsafeFromBool b
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

listProps :: JSRef a -> IO [JSRef JSString]
listProps o = fromArray =<< js_listProps o
{-# INLINE listProps #-}

typeOf :: JSRef a -> IO Int
typeOf r = js_typeOf r
{-# INLINE typeOf #-}

{- | Read a property from a JS object. Throws a 'JSException' if
     o is not a JS object or the property cannot be accessed
 -}
getProp :: ToJSString a => a            -- ^ the property name
                        -> JSRef b      -- ^ the object
                        -> IO (JSRef c) -- ^ the property value
getProp p o = js_getProp (toJSString p) o
{-# INLINE getProp #-}

{- | Read a property from a JS object. Kills the Haskell thread
     if o is not a JS object or the property cannot be accessed
 -}
unsafeGetProp :: ToJSString a => a             -- ^ the property name
                              -> JSRef b       -- ^ the object
                              -> IO (JSRef c)  -- ^ the property value, Nothing if the object doesn't have a property with the given name
unsafeGetProp p o = js_unsafeGetProp (toJSString p) o
{-# INLINE unsafeGetProp #-}

{- | Read a property from a JS object. Throws a JSException if
     o is not a JS object or the property cannot be accessed
 -}
getPropMaybe :: ToJSString a => a                    -- ^ the property name
                             -> JSRef b              -- ^ the object
                             -> IO (Maybe (JSRef c)) -- ^ the property value, Nothing if the object doesn't have a property with the given name
getPropMaybe p o = do
  p' <- js_getProp (toJSString p) o
  if isUndefined p' then return Nothing else return (Just p')
{-# INLINE getPropMaybe #-}

{- | Read a property from a JS object. Kills the Haskell thread
     if o is not a JS object or the property cannot be accessed
 -}
unsafeGetPropMaybe :: ToJSString a => a                    -- ^ the property name
                                   -> JSRef b              -- ^ the object
                                   -> IO (Maybe (JSRef c)) -- ^ the property value, Nothing if the object doesn't have a property with the given name
unsafeGetPropMaybe p o = do
  p' <- js_unsafeGetProp (toJSString p) o
  if isUndefined p' then return Nothing else return (Just p')
{-# INLINE unsafeGetPropMaybe #-}

{- | set a property in a JS object. Throws a 'JSException' if
     o is not a reference to a JS object or the property cannot
     be set
 -}
setProp :: ToJSString a => a       -- ^ the property name
                        -> JSRef b -- ^ the value
                        -> JSRef c -- ^ the object
                        -> IO ()
setProp p v o = js_setProp (toJSString p) v o
{-# INLINE setProp #-}

{- | set a property in a JS object. Kills the Haskell thread
     if the property cannot be set.
-}
unsafeSetProp :: ToJSString a => a       -- ^ the property name
                              -> JSRef b -- ^ the value
                              -> JSRef c -- ^ the object
                              -> IO ()
unsafeSetProp p v o = js_unsafeSetProp (toJSString p) v o

{- | Convert a JavaScript ArrayBuffer to a 'ByteArray' without copying. Throws
     a 'JSException' if the 'JSRef' is not an ArrayBuffer.
 -}
wrapBuffer :: Int          -- ^ offset from the start in bytes, if this is not a multiple of 8,
                           --   not all types can be read from the ByteArray#
           -> Int          -- ^ length in bytes (use zero or a negative number to use the whole ArrayBuffer)
           -> JSRef a      -- ^ JavaScript ArrayBuffer object
           -> IO ByteArray -- ^ result
wrapBuffer offset size buf = unsafeCoerce <$> js_wrapBuffer offset size buf
{-# INLINE wrapBuffer #-}

{- | Convert a JavaScript ArrayBuffer to a 'MutableByteArray' without copying. Throws
     a 'JSException' if the 'JSRef' is not an ArrayBuffer.
 -}
wrapMutableBuffer :: Int          -- ^ offset from the start in bytes, if this is not a multiple of 8,
                                  --   not all types can be read from / written to the ByteArray#
                  -> Int          -- ^ the length in bytes (use zero or a negative number to use the whole ArrayBuffer)
                  -> JSRef a      -- ^ JavaScript ArrayBuffer object
                  -> IO (MutableByteArray s)
wrapMutableBuffer offset size buf = unsafeCoerce <$> js_wrapBuffer offset size buf
{-# INLINE wrapMutableBuffer #-}

{- | Get the underlying JS object from a 'ByteArray#'. The object o
     contains an ArrayBuffer (o.buf) and several typed array views on it (which
     can have an offset from the start of the buffer and/or a reduced length):
      * o.i3 : 32 bit signed
      * o.u8 : 8  bit unsigned
      * o.u1 : 16 bit unsigned
      * o.f3 : 32 bit single precision float
      * o.f6 : 64 bit double precision float
      * o.dv : a DataView
    Some of the views will be null if the offset is not a multiple of 8.
 -}
byteArrayJSRef :: ByteArray# -> JSRef a
byteArrayJSRef a = unsafeCoerce (ByteArray a)
{-# INLINE byteArrayJSRef #-}

{- | Get the underlying JS object from a 'MutableByteArray#'. The object o
     contains an ArrayBuffer (o.buf) and several typed array views on it (which
     can have an offset from the start of the buffer and/or a reduced length):
      * o.i3 : 32 bit signed
      * o.u8 : 8  bit unsigned
      * o.u1 : 16 bit unsigned
      * o.f3 : 32 bit single precision float
      * o.f6 : 64 bit double precision float
      * o.dv : a DataView
    Some of the views will be null if the offset is not a multiple of 8.
 -}
mutableByteArrayJSRef :: MutableByteArray# s -> JSRef a
mutableByteArrayJSRef a = unsafeCoerce (MutableByteArray a)
{-# INLINE mutableByteArrayJSRef #-}

foreign import javascript safe "h$wrapBuffer($3, true, $1, $2)"
  js_wrapBuffer :: Int -> Int -> JSRef a -> IO (JSRef ())

{- | Convert an ArrayBuffer to a strict 'ByteString'
     this wraps the original buffer, without copying.
     Use 'byteArrayByteString' if you already have a wrapped buffer
 -}
bufferByteString :: Int        -- ^ offset from the start in bytes
                 -> Int        -- ^ length in bytes (use zero or a negative number to get the whole ArrayBuffer)
                 -> JSRef a
                 -> IO ByteString
bufferByteString offset length buf = do
  (ByteArray ba) <- wrapBuffer offset length buf
  byteArrayByteString ba

{- | Pack a ByteArray# primitive into a ByteString
     without copying the buffer.

     This is unsafe in native code
 -}
byteArrayByteString :: ByteArray#
                    -> IO ByteString
byteArrayByteString arr =
#ifdef ghcjs_HOST_OS
  let ba        = ByteArray arr
      !(Addr a) = byteArrayContents ba
  in  unsafePackAddressLen (sizeofByteArray ba) a
#else
  error "GHCJS.Foreign.byteArrayToByteString: not JS"
#endif

{- | Pack a MutableByteArray# primitive into a 'ByteString' without
     copying. The byte array shouldn't be modified after converting.

     This is unsafe in native code
 -}
unsafeMutableByteArrayByteString :: MutableByteArray# s
                                 -> IO ByteString
unsafeMutableByteArrayByteString arr =
#ifdef ghcjs_HOST_OS
  let ba        = MutableByteArray arr
      !(Addr a) = mutableByteArrayContents ba
  in  unsafePackAddressLen (sizeofMutableByteArray ba) a
#else
  error "GHCJS.Foreign.unsafeMutableByteArrayToByteString: no JS"
#endif
