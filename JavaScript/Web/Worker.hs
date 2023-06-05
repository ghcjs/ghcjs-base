{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.Worker ( Worker
                             , create
                             , postMessage
                             , terminate
                             ) where

import GHC.JS.Prim

import Data.JSString
import Data.Typeable

newtype Worker = Worker JSVal deriving Typeable

create :: JSString -> IO Worker
create script = js_create script
{-# INLINE create #-}

postMessage :: JSVal -> Worker -> IO ()
postMessage msg w = js_postMessage msg w
{-# INLINE postMessage #-}

terminate :: Worker -> IO ()
terminate w = js_terminate w
{-# INLINE terminate #-}

-- -----------------------------------------------------------------------------

foreign import javascript unsafe 
  "(($1) => { return new Worker($1); })" js_create :: JSString -> IO Worker
foreign import javascript unsafe
  "((x,y) => { y.postMessage(x); })" js_postMessage  :: JSVal -> Worker -> IO ()
foreign import javascript unsafe
  "((x) => { x.terminate(); })" js_terminate :: Worker -> IO ()
