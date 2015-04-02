{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI,
             UnliftedFFITypes, DeriveDataTypeable, MagicHash
  #-}

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
                        , OnBlock (..)
                        , WouldBlockException(..)
                        , synchronously
                        ) where

import           GHCJS.Prim

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as Ex

import           GHC.Exts (ThreadId#)
import           GHC.Conc.Sync (ThreadId(..))

import           Data.Bits (testBit)
import           Data.Data
import           Data.Typeable

import           Unsafe.Coerce

data OnBlock = ContinueAsync
             | ThrowWouldBlock
             deriving (Data, Typeable, Enum, Show, Eq, Ord)

{- |
     Runs the action synchronously, which means that the thread will not
     be preempted by the scheduler. If the thread encounters a blocking
     operation, the scheduler will switch to other threads. When the thread
     is scheduled again, it will still be non-preemptible.

     When the thread encounters a black hole from another thread, the scheduler
     will attempt to clear it by temporarily switching to that thread.
 -}
synchronously :: IO a -> IO a
synchronously x = do
  oldS <- js_setSynchronous True
  if oldS
    then x
    else x `Ex.finally` js_setSynchronous False
{-# INLINE synchronously #-}

makeAsynchronous :: ThreadId -> IO ()
makeAsynchronous (ThreadId tid) = js_makeAsynchronous tid

{- | Returns whether the 'ThreadId' is a synchronous thread
 -}
isThreadSynchronous :: ThreadId -> IO Bool
isThreadSynchronous = fmap (`testBit` 0) . syncThreadState

{- |
     Returns whether the 'ThreadId' will continue running async. Always
     returns 'True' when the thread is not synchronous.
 -}
isContinueAsync :: ThreadId -> IO Bool
isContinueAsync = fmap (`testBit` 1) . syncThreadState

syncThreadState :: ThreadId-> IO Int
syncThreadState (ThreadId tid) = js_syncThreadState tid

-- ----------------------------------------------------------------------------

foreign import javascript unsafe "h$syncThreadState($1)"
  js_syncThreadState :: ThreadId# -> IO Int

foreign import javascript unsafe
  "$r = h$currentThread.isSynchronous;\
  \h$currentThread.isSynchronous = $1;"
  js_setSynchronous :: Bool -> IO Bool

foreign import javascript unsafe
  "$1.isSynchronous = false;"
  js_makeAsynchronous :: ThreadId# -> IO ()
