{-# LANGUAGE DeriveDataTypeable #-}

module JavaScript.Web.Storage.Internal where

import GHCJS.Types

import Data.Typeable

newtype Storage      = Storage JSRef deriving Typeable
newtype StorageEvent = StorageEvent JSRef deriving Typeable
