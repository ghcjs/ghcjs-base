{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, TypeFamilies,
             ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim
  #-}

module Data.JSString.Internal.Search ( indices
                                     ) where

import GHC.Exts (Int#, (+#), Int(..))
import Data.JSString

indices :: JSString -> JSString -> [Int]
indices needle haystack = go 0# 0#
  where
    go n i = case js_indexOf needle n i haystack of
             (# -1#, _  #) -> []
             (# n' , i' #) -> I# n' : go (n' +# 1#) (i' +# 1#) 

foreign import javascript unsafe
  "h$jsstringIndices"
  js_indexOf :: JSString -> Int# -> Int# -> JSString -> (# Int#, Int# #)
