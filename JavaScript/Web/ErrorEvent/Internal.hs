module JavaScript.Web.ErrorEvent.Internal where

import GHCJS.Types

newtype ErrorEvent = ErrorEvent (JSRef ())
