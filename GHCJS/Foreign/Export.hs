{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim, ScopedTypeVariables, UnboxedTuples,
             MagicHash, EmptyDataDecls, CPP
  #-}

{- | 
     Dynamically export Haskell values to JavaScript
 -}

module GHCJS.Foreign.Export
    ( Export
    , export
    , withExport
    , derefExport
    , releaseExport
    ) where

import Control.Exception (bracket)
import GHC.Exts (Any)
import GHC.Fingerprint
import Data.Typeable
import Data.Typeable.Internal (TypeRep(..))
import Data.Word
import Unsafe.Coerce

import GHCJS.Prim

data (Export_ a)
type Export a = JSRef (Export_ a)

{- |
     Export any Haskell value to a JavaScript reference without evaluating it.
     The JavaScript reference can be passed to foreign code and used to retrieve
     the value later.

     The data referenced by the value will be kept in memory until you call
     'releaseExport', even if no foreign code references the export anymore.
 -}
export :: Typeable a => a -> IO (Export a)
export x = js_export w1 w2 (unsafeCoerce x)
  where
#if __GLASGOW_HASKELL__ >= 709
    TypeRep (Fingerprint w1 w2) _ _ _ = typeOf x
#else
    TypeRep (Fingerprint w1 w2) _ _ = typeOf x
#endif

{- |
     Export the value and run the action. The value is only exported for the
     duration of the action. Dereferencing it after the 'withExport' call
     has returned will always return 'Nothing'.
 -}
withExport :: Typeable a => a -> (Export a -> IO b) -> IO b
withExport x m = bracket (export x) releaseExport m

{- |
     Retrieve the Haskell value from an export. Returns 'Nothing' if the
     type does not match or the export has already been released.
 -}

derefExport :: forall a. Typeable a => Export a -> IO (Maybe a)
derefExport e = do
#if __GLASGOW_HASKELL__ >= 709
  let TypeRep (Fingerprint w1 w2) _ _ _ = typeOf (undefined :: a)
#else
  let TypeRep (Fingerprint w1 w2) _ _ = typeOf (undefined :: a)
#endif
  r <- js_derefExport w1 w2 e
  if isNull r
    then return Nothing
    else case js_toHeapObject r of
                (# x #) -> return (Just x)

{- |
     Release all memory associated with the export. Subsequent calls to
     'derefExport' will return 'Nothing'
 -}
releaseExport :: Export a -> IO ()
releaseExport e = js_releaseExport e

-- ----------------------------------------------------------------------------

foreign import javascript unsafe
  "h$exportValue"
  js_export :: Word64 -> Word64 -> Any -> IO (Export a)
foreign import javascript unsafe
  "h$derefExport"
  js_derefExport :: Word64 -> Word64 -> JSRef a -> IO (JSRef ())
foreign import javascript unsafe
  "$r = $1;" js_toHeapObject :: JSRef a -> (# b #)

foreign import javascript unsafe
  "h$releaseExport"
  js_releaseExport :: JSRef a -> IO ()
