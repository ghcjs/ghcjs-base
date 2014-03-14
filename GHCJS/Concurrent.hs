{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, DeriveDataTypeable #-}

{- | GHCJS has two types of threads. Regular, asynchronous threads are
     started with `h$run`, are managed by the scheduler and run in the
     background. `h$run` returns immediately.

     Synchronous threads are started with `h$runSync`, which returns
     when the thread has run to completion. When a synchronous thread
     does an operation that would block, like accessing an MVar or
     an asynchronous FFI call, it cannot continue synchronously.

     There are two ways this can be resolved, depending on the
     second argument of the `h$runSync` call:

      * The action is aborted and the thread receives a 'WouldBlockException'
      * The thread continues asynchronously, `h$runSync` returns

     Note: when a synchronous thread encounters a black hole from
     another thread, it tries to steal the work from that thread
     to avoid blocking. In some cases that might not be possible,
     for example when the data accessed is produced by a lazy IO
     operation. This is resolved the same way as blocking on an IO
     action would.
 -}

module GHCJS.Concurrent ( isThreadSynchronous
                        , isContinueAsync
                        , WouldBlockException(..)
                        ) where

import           GHCJS.Prim

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as Ex

import           Data.Bits (testBit)
import           Data.Typeable

import           Unsafe.Coerce

isThreadSynchronous :: ThreadId -> IO Bool
isThreadSynchronous = fmap (`testBit` 0) . syncThreadState

isContinueAsync :: ThreadId -> IO Bool
isContinueAsync = fmap (`testBit` 1) . syncThreadState

syncThreadState :: ThreadId -> IO Int
syncThreadState tid =
  js_syncThreadState . unsafeCoerce =<<  Ex.evaluate tid

foreign import javascript unsafe "h$syncThreadState($1)"
  js_syncThreadState :: JSRef a -> IO Int

