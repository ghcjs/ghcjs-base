{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module GHCJS.Foreign.Callback.Variadic
  ( VariadicCallback, foldVariadicCb, foldVariadicCbReturn ) where

import GHCJS.Marshal.Pure (PFromJSVal(..))
import GHCJS.Types (JSVal)
import JavaScript.Array (JSArray, (!))
import qualified JavaScript.Array as JA

type Length = Int
type Index = Int

class VariadicCallback a r | a -> r where
  foldVariadicCb' :: Length -> Index -> a -> JSArray -> r

instance VariadicCallback r r where
  foldVariadicCb' len ind a _
    | ind == len = a
    | otherwise = error $ "foldVariadicCb: " ++ (show len) ++
                  " arguments are passed into a haskell callback that "
                  ++ "expects " ++ (show ind) ++ " arguments."

instance (PFromJSVal j, VariadicCallback a r) =>
  VariadicCallback (j -> a) r where
  foldVariadicCb' len ind f arr
    | ind < len =
        foldVariadicCb' len (ind+1) (f $ pFromJSVal $ arr ! ind) arr
    | otherwise = error $ "foldVariadicCb: " ++ (show len)
                  ++ " arguments are passed into a haskell callback that"
                  ++ " expects more arguments"

foldVariadicCb :: VariadicCallback a (IO ()) => a -> JSArray -> IO ()
foldVariadicCb f arr = foldVariadicCb' (JA.length arr) 0 f arr
{-# INLINE foldVariadicCb #-}

foldVariadicCbReturn ::
  VariadicCallback a (IO JSVal) => a -> JSArray -> IO JSVal
foldVariadicCbReturn f arr = foldVariadicCb' (JA.length arr) 0 f arr
{-# INLINE foldVariadicCbReturn #-}
