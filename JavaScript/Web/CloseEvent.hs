{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.CloseEvent ( CloseEvent
                                 , getCode
                                 , getReason
                                 , wasClean
                                 ) where

import Data.JSString

import JavaScript.Web.CloseEvent.Internal

getCode :: CloseEvent -> Int
getCode c = js_getCode c
{-# INLINE getCode #-}

getReason :: CloseEvent -> JSString
getReason c = js_getReason c
{-# INLINE getReason #-}

wasClean :: CloseEvent -> Bool
wasClean c = js_wasClean c
{-# INLINE wasClean #-}

-- -----------------------------------------------------------------------------

foreign import javascript unsafe
  "((x) => { return x.code; })"     js_getCode   :: CloseEvent -> Int
foreign import javascript unsafe
  "((x) => { return x.reason; })"   js_getReason :: CloseEvent -> JSString
foreign import javascript unsafe
  "((x) => { return x.wasClean; })" js_wasClean  :: CloseEvent -> Bool
