{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, DeriveDataTypeable #-}

module JavaScript.Web.Canvas.Pattern {-( Pattern
                                     , Repeat(..)
                                     , create
                                     )-} where

import Data.Typeable
import Data.Data

{-

create :: ...
{-# INLINE create #-}

-- -----------------------------------------------------------------------------

foreign import javascript unsafe js_create
  :: JSString -> IO Pattern
-}
