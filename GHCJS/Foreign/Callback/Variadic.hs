{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GHCJS.Foreign.Callback.Variadic
  ( foldVariadicCb, foldVariadicCbReturn, VariadicCallback ) where

import GHCJS.Types (JSVal, IsJSVal)
import Unsafe.Coerce

class VariadicCallback a r where
  foldVariadicCb' :: a -> [JSVal] -> r

instance VariadicCallback r r where
  foldVariadicCb' a [] = a
  foldVariadicCb' _ _ = error "foldVariadicCb: Wrong number of arguments"

instance (IsJSVal j, VariadicCallback a r) =>
  VariadicCallback (j -> a) r where
  foldVariadicCb' f (x:xs) = foldVariadicCb' (f $ unsafeCoerce x) xs
  foldVariadicCb' _ _ = error "foldVariadicCb: Wrong number of arguments"

foldVariadicCb :: VariadicCallback a (IO ()) => a -> [JSVal] -> IO ()
foldVariadicCb = foldVariadicCb'

foldVariadicCbReturn ::
  VariadicCallback a (IO JSVal) => a -> [JSVal] -> IO JSVal
foldVariadicCbReturn = foldVariadicCb'
