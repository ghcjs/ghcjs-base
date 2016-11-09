{-# LANGUAGE FlexibleInstances #-}
module GHCJS.Foreign.Callback.Variadic where

import GHCJS.Types (JSVal, IsJSVal)
import Unsafe.Coerce

class VariadicCallback a where
  foldVariadicCb :: a -> [JSVal] -> IO ()

instance VariadicCallback (IO ()) where
  foldVariadicCb a [] = a
  foldVariadicCb _ _ = error "foldVariadicCb: Wrong number of arguments"

instance (IsJSVal j, VariadicCallback a) =>
  VariadicCallback (j -> a) where
  foldVariadicCb f (x:xs) = foldVariadicCb (f $ unsafeCoerce x) xs
  foldVariadicCb _ _ = error "foldVariadicCb: Wrong number of arguments"

class VariadicCallbackReturn a where
  foldVariadicCbReturn :: a -> [JSVal] -> IO JSVal

instance VariadicCallbackReturn (IO JSVal) where
  foldVariadicCbReturn a [] = a
  foldVariadicCbReturn _ _ =
    error "foldVariadicCbReturn: Wrong number of arguments"

instance (IsJSVal j, VariadicCallbackReturn a) =>
  VariadicCallbackReturn (j -> a) where
  foldVariadicCbReturn f (x:xs) = foldVariadicCbReturn (f $ unsafeCoerce x) xs
  foldVariadicCbReturn _ _ =
    error "foldVariadicCbReturn: Wrong number of arguments"
