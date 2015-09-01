{-# LANGUAGE DeriveDataTypeable #-}

module GHCJS.Foreign.Callback.Internal where

import GHCJS.Types
import GHCJS.Marshal.Internal

import Data.Typeable

newtype Callback a = Callback JSRef deriving Typeable
instance IsJSRef (Callback a)

