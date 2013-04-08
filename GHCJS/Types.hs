{-# LANGUAGE EmptyDataDecls #-}

module GHCJS.Types ( JSRef
                   , isNull
                   , isUndefined
                   , eqRef
                   , nullRef
                   , castRef
                   , JSString
                   , JSObject
--                   , JSObject'
                   , JSBool
                   , JSNumber
                   )
                 where

import GHCJS.Types.Internal

data JSBool_
data JSNumber_
data JSString_
data JSObject_ a

type JSBool     = JSRef JSBool_
type JSNumber   = JSRef JSNumber_
type JSString   = JSRef JSString_
type JSObject a = JSRef (JSObject_ a)
-- type JSObject'  = JSRef (JSObject (Any *))





