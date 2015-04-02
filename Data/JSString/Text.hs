{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI,
    UnboxedTuples, DeriveDataTypeable, GHCForeignImportPrim,
    MagicHash, FlexibleInstances, BangPatterns, Rank2Types, CPP #-}

{- | Conversion between 'Data.Text.Text' and 'Data.JSString.JSString'

 -}

module Data.JSString.Text
    ( textToJSString
    , textFromJSString
    , lazyTextToJSString
    , lazyTextFromJSString
    , textFromJSRef
    , lazyTextFromJSRef
    ) where

import GHCJS.Prim

import GHC.Exts (ByteArray#, Int(..), Int#, Any)

import Control.DeepSeq

import qualified Data.Text.Array as A
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL

import Data.JSString.Internal.Type

import Unsafe.Coerce

textToJSString :: T.Text -> JSString
textToJSString (T.Text (A.Array ba) (I# offset) (I# length)) =
  js_toString ba offset length
{-# INLINE textToJSString #-}

textFromJSString :: JSString -> T.Text
textFromJSString j =
  case js_fromString j of
    (# _ , 0#     #) -> T.empty
    (# ba, length #) -> T.Text (A.Array ba) 0 (I# length)
{-# INLINE  textFromJSString #-}

lazyTextToJSString :: TL.Text -> JSString
lazyTextToJSString t = rnf t `seq` js_lazyTextToString (unsafeCoerce t)
{-# INLINE lazyTextToJSString #-}

lazyTextFromJSString :: JSString -> TL.Text
lazyTextFromJSString = TL.fromStrict . textFromJSString
{-# INLINE lazyTextFromJSString #-}

-- | returns the empty Text if not a string
textFromJSRef :: JSRef a -> T.Text
textFromJSRef j = case js_fromString' j of
    (# _,  0#     #) -> T.empty
    (# ba, length #) -> T.Text (A.Array ba) 0 (I# length)
{-# INLINE textFromJSRef #-}

-- | returns the empty Text if not a string
lazyTextFromJSRef :: JSRef a -> TL.Text
lazyTextFromJSRef = TL.fromStrict . textFromJSRef
{-# INLINE lazyTextFromJSRef #-}

-- ----------------------------------------------------------------------------

foreign import javascript unsafe
  "h$textToString"
  js_toString :: ByteArray# -> Int# -> Int# -> JSString
foreign import javascript unsafe
  "h$textFromString"
  js_fromString :: JSString -> (# ByteArray#, Int# #)
foreign import javascript unsafe
  "h$textFromString"
  js_fromString' :: JSRef a -> (# ByteArray#, Int# #)
foreign import javascript unsafe
  "h$lazyTextToString"
  js_lazyTextToString :: Any -> JSString
