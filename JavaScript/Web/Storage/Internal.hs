module JavaScript.Web.Storage.Internal where

import GHCJS.Types

newtype Storage      = Storage (JSRef ())
newtype StorageEvent = StorageEvent (JSRef ())
