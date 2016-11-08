{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim, DeriveDataTypeable, GHCForeignImportPrim #-}
{-# LANGUAGE FlexibleContexts #-}
module GHCJS.Foreign.Callback
    ( Callback
    , ToArrayCallback
    , OnBlocked(..)
    , releaseCallback
      -- * asynchronous callbacks
    , asyncCallback
    , asyncCallback1
    , asyncCallback2
    , asyncCallback3
    , asyncCallbackMulti
      -- * synchronous callbacks
    , syncCallback
    , syncCallback1
    , syncCallback2
    , syncCallback3
    , syncCallbackMulti
      -- * synchronous callbacks that return a value
    , syncCallback'
    , syncCallback1'
    , syncCallback2'
    , syncCallback3'
    , syncCallbackMulti'
    ) where

import           GHCJS.Concurrent
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Foreign.Callback.Internal
import           GHCJS.Foreign.Callback.ToArrayCallback
import           GHCJS.Prim
import           GHCJS.Types

import qualified GHC.Exts as Exts

import           Data.Typeable

import           Unsafe.Coerce

{- | When you create a callback, the Haskell runtime stores a reference to
     the exported IO action or function. This means that all data referenced
     by the exported value stays in memory, even if nothing outside the
     Haskell runtime holds a reference to to callback.

     Use 'releaseCallback' to free the reference. Subsequent calls from
     JavaScript to the callback will result in an exception.
 -}
releaseCallback :: Callback a -> IO ()
releaseCallback x = js_release x

{- | Make a callback (JavaScript function) that runs the supplied IO action in
     a synchronous thread when called.

     Call 'releaseCallback' when done with the callback, freeing memory
     referenced by the IO action.
 -}
syncCallback :: OnBlocked -- ^ what to do when the thread blocks
             -> IO ()     -- ^ the Haskell action
             -> IO (Callback (IO ())) -- ^ the callback
syncCallback onBlocked x =
  js_syncCallback (onBlocked == ContinueAsync) (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied IO function
     in a synchronous thread when called. The callback takes one argument that
     it passes as a JSVal value to the Haskell function.

     Call 'releaseCallback' when done with the callback, freeing data
     referenced by the function.
 -}
syncCallback1 :: OnBlocked -- ^ what to do when the thread blocks
              -> (JSVal -> IO ()) -- ^ the Haskell function
              -> IO (Callback (JSVal -> IO ())) -- ^ the callback
syncCallback1 onBlocked x =
  js_syncCallbackApply (onBlocked == ContinueAsync) 1 (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied IO function
     in a synchronous thread when called. The callback takes two arguments
     that it passes as JSVal values to the Haskell function.

     Call 'releaseCallback' when done with the callback, freeing data
     referenced by the function.
 -}
syncCallback2 :: OnBlocked -- ^ what to do when the thread blocks
              -> (JSVal -> JSVal -> IO ()) -- ^ the Haskell function
              -> IO (Callback (JSVal -> JSVal -> IO ())) -- ^ the callback
syncCallback2 onBlocked x =
  js_syncCallbackApply (onBlocked == ContinueAsync) 2 (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied IO function
     in a synchronous thread when called. The callback takes three arguments
     that it passes as JSVal values to the Haskell function.

     Call 'releaseCallback' when done with the callback, freeing data
     referenced by the function.
 -}
syncCallback3 :: OnBlocked -- ^ what to do when the thread blocks
              -> (JSVal -> JSVal -> JSVal -> IO ()) -- ^ the Haskell function
              -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
              -- ^ the callback
syncCallback3 onBlocked x =
  js_syncCallbackApply (onBlocked == ContinueAsync) 3 (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied haskell
     function in a synchronous thread when called.

     The haskell function is a polyvariadic function that behaves like
     IsJSVal a => a -> ... -> a -> IO (). In other words, the function takes
     one or more arguments of an IsJSVal instance and returns IO ().

     Call 'releaseCallback' on the callback when done with the callback,
     freeing data referenced by the function.
 -}
syncCallbackMulti :: ToArrayCallback f () =>
  OnBlocked -- ^ what to do when the thread blocks
  -> f -- ^ the Haskell function
  -> IO (Callback f) -- ^ the Callback
syncCallbackMulti onBlocked f = do
  js_syncCallbackMulti (onBlocked == ContinueAsync) $ unsafeCoerce $ \args ->
    fromJSArray args >>= toArrayCallback f

{- | Make a callback (JavaScript function) that runs the supplied IO action in
     a synchronous thread when called. The callback returns JSVal.

     When the thread is blocked, it throws an
     `GHCJS.Concurrent.WouldBlockException` exception.

     Call 'releaseCallback' when done with the callback, freeing memory
     referenced by the IO action.
 -}
syncCallback' :: IO JSVal
              -> IO (Callback (IO JSVal))
syncCallback' x = js_syncCallbackReturn (unsafeCoerce x)

syncCallback1' :: (JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> IO JSVal))
syncCallback1' x = js_syncCallbackApplyReturn 1 (unsafeCoerce x)

syncCallback2' :: (JSVal -> JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> JSVal -> IO JSVal))
syncCallback2' x = js_syncCallbackApplyReturn 2 (unsafeCoerce x)

syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> JSVal -> JSVal -> IO JSVal))
syncCallback3' x = js_syncCallbackApplyReturn 3 (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied IO function
     in a synchronous thread when called.

     The haskell function is a polyvariadic function that behaves like
     IsJSVal a => a -> ... -> a -> IO JSVal. In other words, the function
     takes one or more arguments of an IsJSVal instance and returns IO JSVal
     to Javascript.

     When the thread is blocked, it throws an
     `GHCJS.Concurrent.WouldBlockException` exception.

     Call 'releaseCallback' on the callback when done with the callback,
     freeing data referenced by the function.
 -}
syncCallbackMulti' :: ToArrayCallback f JSVal =>
                      f -- ^ the Haskell function
                   -> IO (Callback f) -- ^ the callback
syncCallbackMulti' f = js_syncCallbackMultiReturn $ unsafeCoerce $ \args ->
  fromJSArray args >>= toArrayCallback f

{- | Make a callback (JavaScript function) that runs the supplied IO action
     in an asynchronous thread when called.

     Call 'releaseCallback' when done with the callback, freeing data
     referenced by the IO action.
 -}
asyncCallback :: IO () -- ^ the action that the callback runs
              -> IO (Callback (IO ())) -- ^ the callback
asyncCallback x = js_asyncCallback (unsafeCoerce x)

asyncCallback1 :: (JSVal -> IO ()) -- ^ the function that the callback calls
               -> IO (Callback (JSVal -> IO ())) -- ^ the calback
asyncCallback1 x = js_asyncCallbackApply 1 (unsafeCoerce x)

asyncCallback2 :: (JSVal -> JSVal -> IO ())
                  -- ^ the Haskell function that the callback calls
               -> IO (Callback (JSVal -> JSVal -> IO ())) -- ^ the callback
asyncCallback2 x = js_asyncCallbackApply 2 (unsafeCoerce x)

asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ())
                  -- ^ the Haskell function that the callback calls
               -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
                  -- ^ the callback
asyncCallback3 x = js_asyncCallbackApply 3 (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied IO function
     in an asynchronous thread when called.

     The haskell function is a polyvariadic function that behaves like
     IsJSVal a => a -> ... -> a -> IO (). In other words, the function takes
     one or more arguments of an IsJSVal instance and returns IO ().

     Call 'releaseCallback' on the callback when done with the callback,
     freeing data referenced by the function.
 -}
asyncCallbackMulti :: ToArrayCallback f () =>
                      f -- ^ the Haskell functionn
                   -> IO (Callback f) -- ^ the callback
asyncCallbackMulti f = js_asyncCallbackMulti $ unsafeCoerce $ \args ->
  fromJSArray args >>= toArrayCallback f

-- ----------------------------------------------------------------------------

foreign import javascript unsafe "h$makeCallback(h$runSync, [$1], $2)"
  js_syncCallback :: Bool -> Exts.Any -> IO (Callback (IO b))
foreign import javascript unsafe "h$makeCallback(h$run, [], $1)"
  js_asyncCallback :: Exts.Any -> IO (Callback (IO b))
foreign import javascript unsafe "h$makeCallback(h$runSyncReturn, [false], $1)"
  js_syncCallbackReturn :: Exts.Any -> IO (Callback (IO JSVal))

foreign import javascript unsafe "h$makeCallbackApply($2, h$runSync, [$1], $3)"
  js_syncCallbackApply :: Bool -> Int -> Exts.Any -> IO (Callback b)
foreign import javascript unsafe "h$makeCallbackApply($1, h$run, [], $2)"
  js_asyncCallbackApply :: Int -> Exts.Any -> IO (Callback b)
foreign import javascript unsafe
  "h$makeCallbackApply($1, h$runSyncReturn, [false], $2)"
  js_syncCallbackApplyReturn :: Int -> Exts.Any -> IO (Callback b)

foreign import javascript unsafe "h$makeCallbackMulti(h$runSync, [$1], $2)"
  js_syncCallbackMulti :: Bool -> Exts.Any -> IO (Callback a)
foreign import javascript unsafe "h$makeCallbackMulti(h$run, [], $1)"
  js_asyncCallbackMulti :: Exts.Any -> IO (Callback a)
foreign import javascript unsafe
  "h$makeCallbackMulti(h$runSyncReturn, [false], $1)"
  js_syncCallbackMultiReturn :: Exts.Any -> IO (Callback a)

foreign import javascript unsafe "h$release"
  js_release :: Callback a -> IO ()
