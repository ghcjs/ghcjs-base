{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

module JavaScript.Web.Blob.Internal where

import Data.Typeable

import GHCJS.Types

data BlobType = BlobTypeBlob
              | BlobTypeFile

newtype SomeBlob (a :: BlobType) = SomeBlob JSVal deriving Typeable

type File = SomeBlob BlobTypeFile
type Blob = SomeBlob BlobTypeBlob
  
size :: SomeBlob a -> Int
size b = js_size b
{-# INLINE size #-}

contentType :: SomeBlob a -> JSString
contentType b = js_type b
{-# INLINE contentType #-}

-- is the type correct, does slicing a File give another File?
slice :: Int -> Int -> JSString -> SomeBlob a -> SomeBlob a
slice start end contentType b = js_slice start end contentType b
{-# INLINE slice #-}

isClosed :: SomeBlob a -> IO Bool
isClosed b = js_isClosed b
{-# INLINE isClosed #-}

close :: SomeBlob a -> IO ()
close b = js_close b
{-# INLINE close #-}

-- -----------------------------------------------------------------------------

foreign import javascript unsafe "((x) => { return x.size; })" js_size :: SomeBlob a -> Int
foreign import javascript unsafe "((x) => { return x.type; })" js_type :: SomeBlob a -> JSString

-- fixme figure out if we need to support older browsers with obsolete slice
foreign import javascript unsafe "(($1,$2,$3,$4) => { return $4.slice($1,$2,$3); })"
  js_slice :: Int -> Int -> JSString -> SomeBlob a -> SomeBlob a

foreign import javascript unsafe "((x) => { return x.isClosed; })"
  js_isClosed :: SomeBlob a -> IO Bool
foreign import javascript unsafe "((x) => { return x.close(); })"
  js_close :: SomeBlob a -> IO ()
