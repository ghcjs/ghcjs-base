# Introduction to ghcjs-base

It is a minimal low-level base library for GHCJS, used by higher level
libraries like JSC

It contains modules for 

* Marshalling from and to Javascript in `GHCJS/`
* Javascript API in `JavaScript/`

# Marshalling from and to Javascript

This section assumes you read *GHCJS foreign function interface* which you can find on *ghcjs/ghcjs*.

### Representation of Javascript Types in GHCJS

#### Arbitrary Javascript Value

Arbitrary javascript values are represented by `GHCJS.Types.JSVal`

```haskell
import GHCJS.Types (JSVal)

foreign import javascript unsafe
  "$1 + $2" add :: JSVal -> Int -> JSVal

foreign import javascript unsafe
  "require($1)" require :: JSVal -> JSVal
```

Internally, `JSVal` is defined as

```haskell
data JSVal = JSVal ByteArray#
```

But, it is an implementation detail you should not have to care about in most cases.

#### Javascript String

It is defined in `Data.JSString` as

```haskell
newtype JSString = JSString JSVal
```

`JSString` is also available from `GHCJS.Types`.

#### TypedArray

If you already knew how to use TypedArray, understanding `JavaScript.TypedArray`
would not be difficult.

#### ArrayBuffer

It is represented by `JavaScript.TypedArray.ArrayBuffer`.
`GHCJS.Buffer` is an obsolete implementation of ArrayBuffer.

### Conversion among Javascript Types

It seems people use `unsafeCoerce` from `Unsafe.Coerce` to convert `JSVal` to other Javascript types and vice versa. There is `JavaScript.Cast`, but it is considered incomplete.

### Conversion between Haskell String and Javascript String

`Data.JSString` contains

```haskell
pack :: String -> JSString
unpack :: JSString -> String
```

Use them as in the following example.

```haskell
import Data.JSString as S

foreign import javascript unsafe
  "require('console').log($1)" console_log :: S.JSString -> IO ()

main :: IO ()
main = do
  let message = "yo" :: String
  console_log (S.pack message)
```

### Pass Haskell String Literals as Javascript String

`Data.JSString` contains `instance IsString JSString`, so it's possible to write

```haskell
import Data.JSString as S

foreign import javascript unsafe
  "require('console').log($1)" console_log :: S.JSString -> IO ()

main :: IO ()
main = do
  console_log "yo"
```

### Marshalling Arbitrary Types from and to Javascript

GHCJS FFI by itself cannot marshal a lot of types from and to `JSVal`. If you want to convert `JSVal` into such types, you need to make wrapper functions that call foreign functions, convert the result to a value of the desired type, and return the converted value. Here is a minimal example.

```haskell
foreign import javascript unsafe
  "$1 === 0" js_isEqualToZero :: Int -> Bool

data NewBool = Yes | No

isEqualToZero :: Int -> NewBool
isEqualToZero n = if js_isEqualToZero n then Yes else No
```

#### Pure Marshalling

If the conversion from and to `JSVal` doesn't involve side effects, you can use pure marshalling API.
`GHCJS.Marshal.Pure` exports

```haskell
class PToJSVal a where
  pToJSVal :: a -> JSVal

class PFromJSVal a where
  pFromJSVal :: JSVal -> a
```

`GHCJS.Marshal.Pure` has `PFromJSVal` and `PToJSVal` instances for various basic types.

#### Impure Marshalling

If the conversion from and to `JSVal` involves side effects or doesn't return
the same output every time for the same input, you may want to use
`GHCJS.Marshal`. `GHCJS.Marshal` exports

```haskell
import qualified Data.Aeson as AE

toJSVal_aeson :: AE.ToJSON a => a -> IO JSVal
toJSVal_pure :: PToJSVal a => a -> IO JSVal

class ToJSVal a where
  toJSVal :: a -> IO JSVal
  -- other functions are omitted for simplicity

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)
  -- other functions are omitted for simplicity
```

As far as I know, since `FromJSVal` and `ToJSVal` are generic typeclasses, you can use Generics to derive instances for `FromJSVal` and `ToJSVal` without boiler plates if you know how to use Generics.

#### `Maybe` in Javascript Functions

If you want to express `Maybe` in imported foreign functions, use `GHCJS.Nullable`. `Nullable` is defined in `GHCJS.Nullable` as

```haskell
newtype Nullable = Nullable JSVal
```

It is simply a newtype wrapper around `JSVal`. The following functions turn `Nullable` into something more than a mere newtype wrapper around `JSVal`.
`GHCJS.Nullable` exports

```haskell
nullableToMaybe :: PFromJSVal a => Nullable a -> Maybe a
maybeToNullable :: PToJSVal a => Maybe a -> Nullable a
```

The type signatures of those function make it clear that you need to implement pure marshalling typeclasses if you want to marshal `Maybe`.

You can use `Nullable` as below.

```haskell
import GHCJS.Nullable

foreign import javascript unsafe
  "if($1 === 0) { $r = null; } else { $r=$1; }"
  js_nullIfZero :: Int -> Nullable Int

maybeZero :: Int -> Maybe Int
maybeZero n = nullableToMaybe (js_nullIfZero n)
```

In the above example, I didn't need `instance PFromJSVal Int` because it is already implemented in `GHCJS.Marshal.Pure`. As stated before, `GHCJS.Marshal.Pure` has `PFromJSVal` and `PToJSVal` instances for many basic types.

### Passing Haskell Callbacks to Javascript

With `GHCJS.Foreign.Callback`, you can create callbacks that you can pass to imported javascript functions. `Callback` is defined as

```haskell
newtype Callback a = Callback JSVal
```

It's just a newtype wrapper around `JSVal`. There are currently two kinds of callbacks, synchronous callbacks and asynchronous callbacks.

#### Asynchronous Callbacks

Asynchronous callbacks are simpler than synchronous callbacks. If an synchronous callback is passed to a javascript function and the function calls the callback, the callback launches an asynchronous haskell thread. Let's look at the functions that generate asynchronous callbacks.

```haskell
asyncCallback :: IO () -> IO (Callback (IO ()))
asyncCallback1 :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
asyncCallback2 :: (JSVal -> JSVal -> IO ())
               -> IO (Callback (JSVal -> JSVal -> IO ()))

-- There is also asyncCallback3
```

`asyncCallback` accepts an IO action and returns a callback in `IO`. `asyncCallback1` returns a callback that accepts one argument. `asyncCallback2` for a callback of 2 arguments. You can guess what `asyncCallback3` is for.

#### Synchronous Callbacks

When a synchronous callback is called in javascript, it launches a new synchronous thread. There are two kinds of synchronous callback.

* Synchronous callback that throws `GHCJS.Concurrent.WouldBlockException` when its thread blocks
* One that becomes asynchronous when the thread blocks

Let's look at the functions that generate synchronous callbacks.

```haskell
syncCallback :: OnBlocked -- ^ what to do when the thread blocks
             -> IO () -- ^ the Haskell action
             -> IO (Callback (IO ())) -- ^ the callback
```

It's almost the same as `asyncCallback` except the argument for `OnBlocked`. `OnBlocked` is defined in `GHCJS.Concurrent` as

```haskell
data OnBlocked = ContinueAsync -- ^ continue the thread asynchronously if blocked
               | ThrowWouldBlock -- ^ throw 'WouldBlockException' if blocked
               deriving (Data, Typeable, Enum, Show, Eq, Ord)
```

`OnBlocked` is exported from `GHCJS.Foreign.Callback`, so you don't have to
import `GHCJS.Concurretn` separately.
You can guess what `syncCallback1`, `syncCallback2`, and `syncCallback3` do.

`syncCallback'`, `syncCallback1'`, and so on generate synchronous callbacks that
throw `WouldBlockException` when their threads block. It's almost the same as
`syncCallback ThrowWouldBlock`, `syncCallback1 ThrowWouldBlock`, and so on.
But, the generated callbacks return `IO JSVal`.

```haskell
syncCallback' :: IO JSVal
              -> IO (Callback (IO JSVal))
syncCallback1' :: (JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> IO JSVal))
syncCallback2' :: (JSVal -> JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> JSVal -> IO JSVal))
syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> JSVal -> JSVal -> IO JSVal))
```

#### An Example of Using Callback in NodeJs

```haskell
import GHCJS.Foreign.Callback
import Data.JSString -- This includes an IsString instance for JSString
import GHCJS.Types (JSVal)

foreign import javascript unsafe
  "require('console').log($1)" js_consoleLog :: JSVal -> IO ()

foreign import javascript unsafe
  "require('fs').stat($1, $2)"
  js_fsStat :: JSString -> Callback (JSVal -> JSVal -> IO ()) -> IO ()

main :: IO ()
main = do
  cb <- asyncCallback2 $ \err stat -> js_consoleLog stat
  js_fsStat "/home" cb
  releaseCallback cb
```

#### Callbacks with Arbitrary Numbers of Arguments

There is a multi version for each type of callback generator.

```haskell
syncCallbackMulti :: OnBlocked -> ([JSVal] -> IO ())
                  -> IO (Callback ([JSVal] -> IO ()))
syncCallbackMulti' :: ([JSVal] -> IO JSVal)
                   -> IO (Callback ([JSVal] -> IO JSVal))
asyncCallbackMulti :: ([JSVal] -> IO ()) -> IO (Callback ([JSVal] -> IO ()))
```

Each of them makes a callback (JavaScript function) that runs the supplied
function. The callback takes an arbitrary number of arguments that it passes
as an array of JSVal values to the Haskell function. The following NodeJS
example shows how to use `asyncCallbackMulti`. You can adapt the following
nodejs example to `syncCallbackMulti` and `syncCallbackMulti'`.


```haskell
{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, LambdaCase #-}
module Main where

import qualified GHCJS.Foreign.Callback as C
import GHCJS.Types
import qualified Control.Concurrent.MVar as M
import GHCJS.Prim (toJSArray)
import Data.JSString (unpack)

foreign import javascript unsafe
  "$1(1, 'abc', 3.5)"
  js_testCallback :: C.Callback ([JSVal] -> IO ()) -> IO ()

foreign import javascript unsafe
  "'' + $1"
  js_toJSString :: JSVal -> JSString

printJsval :: JSVal -> IO ()
printJsval = putStrLn . unpack . js_toJSString

main :: IO ()
main = do
  putStrLn "Test asyncCallbackMulti"
  comm <- M.newEmptyMVar
  cb <- C.asyncCallbackMulti $ \case
    [a, b, c] -> do
      printJsval a
      printJsval b
      printJsval c
      M.putMVar comm "MVar box cat 1"
    args -> do
      print "Unexpected number of arguments : "
      toJSArray args >>= printJsval
      M.putMVar comm "MVar box cat 2"
  js_testCallback cb
  M.takeMVar comm >>= putStrLn
```

#### Caveats on Callbacks

* All callbacks should be manually released from memory by `releaseCallback` later.

### How can Arbitrary Haskell Value be stored in and retrieved from Javascript Data Structures?

For example, if you wanted arbitrary haskell data structures to be stored in and retrieved from javascript hashmap, 
`GHCJS.Foreign.Export` should be used. `GHCJS.Foreign.Export` exports

```haskell
newtype Export a = Export JSVal

export :: Typeable a => a -> IO (Export a)
withExport :: Typeable a => a -> (Export a -> IO b) -> IO b
derefExport :: forall a. Typeable a => Export a -> IO (Maybe a)
releaseExport :: Export a -> IO ()
```

* `export` : exports an arbitrary haskell data structure as a blob of javascript value. Since `Export a` is actually `JSVal`, `Export a` can be passed to and retrieved from javascript data structures such as hashmap.
* `releaseExport` : releases all memory associated with the export. Subsequent calls to 'derefExport' will return 'Nothing'
* `derefExport` : retrieves the haskell value from an export. It returns 'Nothing' if the type does not match or the export has already been released.
* `withExport` : exports a given value, runs the action `IO b`, and returns `IO b`. The value is only exported for the duration of the action. Dereferencing it after the `withExport` call has returned will always return `Nothing`.

### Various Utilities for Javascript

`GHCJS.Foreign` exports `jsTrue`, `jsFalse`, `jsNull`, `isTruthy`, `isString`, `isBoolean`, etc, ... You can inspect those functions and understand them easily.

# Javascript API

If you already knew javascript APIs, it wouldn't be difficult to inspect and understand `JavaScript.Object`, `JavaScript.Array`, `JavaScript.String`, `JavaScript.RegExp`, etc, ...