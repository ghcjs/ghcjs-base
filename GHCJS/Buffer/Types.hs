{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}

module GHCJS.Buffer.Types where

import GHCJS.Types
import GHCJS.Internal.Types

newtype SomeBuffer (a :: MutabilityType s) = SomeBuffer JSRef

type    Buffer         = SomeBuffer Immutable
type    MutableBuffer  = SomeBuffer Mutable
