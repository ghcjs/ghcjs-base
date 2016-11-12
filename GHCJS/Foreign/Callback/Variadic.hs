{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module GHCJS.Foreign.Callback.Variadic where

import GHCJS.Types (JSVal)
import GHCJS.Marshal.Pure (PFromJSVal(..))

class VariadicCallback a r | a -> r where
  foldVariadicCb :: a -> [JSVal] -> r

instance VariadicCallback r r where
  foldVariadicCb a [] = a
  foldVariadicCb _ _ = error "foldVariadicCb: Wrong number of arguments"

instance (PFromJSVal j, VariadicCallback a r) =>
  VariadicCallback (j -> a) r where
  foldVariadicCb f (x:xs) = foldVariadicCb (f $ pFromJSVal x) xs
  foldVariadicCb _ _ = error "foldVariadicCb: Wrong number of arguments"
