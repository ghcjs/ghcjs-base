{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}

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
import qualified GHC.Exts as Exts

import GHCJS.Prim

type Export a = JSVal

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
    TypeRep (Fingerprint w1 w2) _ _ _ = typeOf x

{- |
     Export the value and run the action. The value is only exported for the
     duration of the action. Dereferencing it after the 'withExport' call
     has returned will always return 'Nothing'.
 -}
-- fixme is this safe with nested exports?
withExport :: Typeable a => a -> (Export a -> IO b) -> IO b
withExport x m = bracket (export x) releaseExport m

{- |
     Retrieve the Haskell value from an export. Returns 'Nothing' if the
     type does not match or the export has already been released.
 -}

derefExport :: forall a. Typeable a => Export a -> IO (Maybe a)
derefExport e = do
  let TypeRep (Fingerprint w1 w2) _ _ _ = typeOf (undefined :: a)
  r <- js_derefExport w1 w2 e
  if isNull r
    then return Nothing
    else unsafeCoerce (js_toHeapObject r)

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
  js_derefExport :: Word64 -> Word64 -> JSVal -> IO JSVal
foreign import javascript unsafe
  "$r = $1;" js_toHeapObject :: JSVal -> Exts.Any

foreign import javascript unsafe
  "h$releaseExport"
  js_releaseExport :: JSVal -> IO ()
