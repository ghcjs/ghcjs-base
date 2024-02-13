{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.Canvas.ImageData ( ImageData
                                       , width
                                       , height
                                       , getData
                                       ) where

import JavaScript.TypedArray

import JavaScript.Web.Canvas.Internal

height :: ImageData -> Int
height i = js_height i
{-# INLINE height #-}

width :: ImageData -> Int
width i = js_width i
{-# INLINE width #-}

getData :: ImageData -> Uint8ClampedArray
getData i = js_getData i
{-# INLINE getData #-}

-- -----------------------------------------------------------------------------

foreign import javascript unsafe
  "((x) => { return x.width; })" js_width :: ImageData -> Int
foreign import javascript unsafe
  "((x) => { return x.height; })" js_height :: ImageData -> Int
foreign import javascript unsafe
  "((x) => { return x.data; })" js_getData :: ImageData -> Uint8ClampedArray

