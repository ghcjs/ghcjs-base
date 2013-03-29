{-# LANGUAGE EmptyDataDecls #-}

module GHCJS.Types where

import Foreign.Ptr

data JSChar
type JSCString = Ptr JSChar

