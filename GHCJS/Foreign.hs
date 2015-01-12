{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure

import           Data.String (IsString(..))
import qualified Data.Text as T

class (PToJSRef a, ToJSRef a) => ToJSString a where
  toJSString :: a -> JSString
  toJSString = castRef . ptoJSRef
  {-# INLINE toJSString #-}

class (PFromJSRef a, FromJSRef a) => FromJSString a where
  fromJSString :: JSString -> a
  fromJSString = pfromJSRef . castRef
  {-# INLINE fromJSString #-}

instance ToJSString [Char]
instance FromJSString [Char]
instance ToJSString T.Text
instance FromJSString T.Text
instance ToJSString JSString
instance FromJSString JSString

instance IsString JSString where
  fromString = toJSString
  {-# INLINE fromString #-}

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

