{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.File ( File
                             -- Blob operations
                           , size
                           , contentType
                           , slice
                           , isClosed
                           , close
                             -- additional File operations
                           , name
                           , lastModified
                           ) where

import JavaScript.Web.Blob.Internal

import Data.JSString

name :: File -> JSString
name b = js_name b
{-# INLINE name #-}

lastModified :: File -> Double
lastModified b = js_lastModified b
{-# INLINE lastModified #-}

-- -----------------------------------------------------------------------------

foreign import javascript unsafe "((x) => { return x.name; })"         js_name         :: File -> JSString
foreign import javascript unsafe "((x) => { return x.lastModified; })" js_lastModified :: File -> Double

