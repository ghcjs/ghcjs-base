{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, TypeFamilies,
             ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim
  #-}

module Data.JSString.Internal.Search ( indices
                                     ) where

import GHC.Exts (Int#, (+#), Int(..))
import Data.JSString

-- returns uncorrected offsets in the String
indices :: JSString -> JSString -> [Int]
indices needle haystack = go 0#
  where
    go i = case js_indexOf needle i haystack of
             -1# -> []
             n   -> I# n : go (n +# 1#)

foreign import javascript unsafe
  "((x,y,z) => { return z.indexOf(x,y); })"
  js_indexOf :: JSString -> Int# -> JSString -> Int#
