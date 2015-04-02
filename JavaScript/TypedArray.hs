module JavaScript.TypedArray
    ( TypedArray(..)
    , Int8Array, Int16Array, Int32Array
    , Uint8Array, Uint16Array, Uint32Array
    , Uint8ClampedArray
    , length
    , byteLength
    , byteOffset
    , buffer
    , subarray
    , set
    , unsafeSet
    ) where

import Prelude ()

import JavaScript.TypedArray.Internal
import JavaScript.TypedArray.Internal.Types

