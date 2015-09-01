{-# LANGUAGE EmptyDataDecls #-}

module JavaScript.Web.Canvas.Internal ( Canvas(..)
                                      , Context(..)
                                      , Gradient(..)
                                      , Image(..)
                                      , ImageData(..)
                                      , Pattern(..)
                                      , TextMetrics(..)
                                      ) where

import GHCJS.Types

newtype Canvas      = Canvas      JSRef
newtype Context     = Context     JSRef
newtype Gradient    = Gradient    JSRef
newtype Image       = Image       JSRef
newtype ImageData   = ImageData   JSRef
newtype Pattern     = Pattern     JSRef
newtype TextMetrics = TextMetrics JSRef

instance IsJSRef Canvas
instance IsJSRef Context
instance IsJSRef Gradient
instance IsJSRef Image
instance IsJSRef ImageData
instance IsJSRef Pattern
instance IsJSRef TextMetrics
