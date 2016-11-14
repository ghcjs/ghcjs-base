{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GHCJS.Foreign.Callback.Variadic
  ( VariadicCallback
  , asyncCallbackVar
  , syncCallbackVar
  , syncCallbackVar'
  ) where

import GHCJS.Marshal.Pure (PFromJSVal(..))
import GHCJS.Types (JSVal)
import JavaScript.Array (JSArray, (!))
import qualified JavaScript.Array as JA
import GHCJS.Foreign.Callback

import Unsafe.Coerce
import qualified GHC.Exts as Exts

type Length = Int
type Index = Int

class VariadicCallback a r where
  foldVariadicCb' :: Length -> Index -> a -> JSArray -> r

instance VariadicCallback r r where
  foldVariadicCb' len ind a _
    | ind == len = a
    | otherwise = error $ "foldVariadicCb: " ++ (show len) ++
                  " arguments are passed into a haskell callback that "
                  ++ "expects " ++ (show ind) ++ " arguments."

instance (PFromJSVal j, VariadicCallback a r) =>
  VariadicCallback (j -> a) r where
  foldVariadicCb' len ind f arr
    | ind < len =
        foldVariadicCb' len (ind+1) (f $ pFromJSVal $ arr ! ind) arr
    | otherwise = error $ "foldVariadicCb: " ++ (show len)
                  ++ " arguments are passed into a haskell callback that"
                  ++ " expects more arguments"

foldVariadicCb :: VariadicCallback a (IO ()) => a -> JSArray -> IO ()
foldVariadicCb f arr = foldVariadicCb' (JA.length arr) 0 f arr

foldVariadicCbReturn ::
  VariadicCallback a (IO JSVal) => a -> JSArray -> IO JSVal
foldVariadicCbReturn f arr = foldVariadicCb' (JA.length arr) 0 f arr

{- | Make a variadic callback (JavaScript function) that runs the supplied
     haskell function in a synchronous thread when called.

     The types of the supplied haskell function can be `IO ()`,
     `PFromJSVal j => j -> IO ()`,
     `(PFromJSVal j, PFromJSVal j2) => j -> j2 -> IO ()`,
     and so on.

     Call 'releaseCallback' on the callback when done with the callback,
     freeing data referenced by the function.
 -}
syncCallbackVar :: VariadicCallback f (IO ()) =>
                   OnBlocked -- ^ what to do when the thread blocks
                -> f -- ^ the Haskell function
                -> IO (Callback f) -- ^ the Callback
syncCallbackVar onBlocked f = js_syncCallbackVar
  (onBlocked == ContinueAsync) $ unsafeCoerce $ foldVariadicCb f

{- | Make a variadic callback (JavaScript function) that runs the supplied
     haskell function in a synchronous thread when called.

     The types of the supplied haskell function can be `IO JSVal`,
     `PFromJSVal j => j -> IO JSVal`,
     `(PFromJSVal j, PFromJSVal j2) => j -> j2 -> IO JSVal`, and so on.
     When the thread is blocked, it throws an
     `GHCJS.Concurrent.WouldBlockException` exception.

     Call 'releaseCallback' on the callback when done with the callback,
     freeing data referenced by the function.
 -}
syncCallbackVar' :: VariadicCallback f (IO JSVal) =>
                    f -- ^ the haskell function
                 -> IO (Callback f) -- ^ the callback
syncCallbackVar' f =
  js_syncCallbackVarReturn $ unsafeCoerce $ foldVariadicCbReturn f

{- | Make a variadic callback (JavaScript function) that runs the supplied
     haskell function in an asynchronous thread when called.

     The types of the supplied haskell function can be `IO ()`,
     `PFromJSVal j => j -> IO ()`,
     `(PFromJSVal j, PFromJSVal j2) => j -> j2 -> IO ()`,
     and so on.

     Call 'releaseCallback' on the callback when done with the callback,
     freeing data referenced by the function.
 -}
asyncCallbackVar :: VariadicCallback f (IO ()) =>
                    f -- ^ the Haskell function
                 -> IO (Callback f) -- ^ the callback
asyncCallbackVar f = js_asyncCallbackVar $ unsafeCoerce $ foldVariadicCb f

foreign import javascript unsafe "h$makeCallbackVar(h$runSync, [$1], $2)"
  js_syncCallbackVar :: Bool -> Exts.Any -> IO (Callback a)
foreign import javascript unsafe "h$makeCallbackVar(h$run, [], $1)"
  js_asyncCallbackVar :: Exts.Any -> IO (Callback a)
foreign import javascript unsafe
  "h$makeCallbackVar(h$runSyncReturn, [false], $1)"
  js_syncCallbackVarReturn :: Exts.Any -> IO (Callback a)
