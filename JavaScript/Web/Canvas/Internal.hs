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

data Canvas_
data Context_
data Pattern_
data Gradient_
data Image_
data ImageData_
data TextMetrics_

newtype Canvas      = Canvas      (JSRef Canvas_)
newtype Context     = Context     (JSRef Context_)
newtype Gradient    = Gradient    (JSRef Gradient_)
newtype Image       = Image       (JSRef Image_)
newtype ImageData   = ImageData   (JSRef ImageData_)
newtype Pattern     = Pattern     (JSRef Pattern_)
newtype TextMetrics = TextMetrics (JSRef TextMetrics_) 

