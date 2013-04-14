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
                   , JSArray
                   )
                 where

import GHCJS.Types.Internal

data JSBool_
data JSNumber_
data JSString_
data JSObject_ a
data JSArray_ a

type JSBool     = JSRef JSBool_
type JSNumber   = JSRef JSNumber_
type JSString   = JSRef JSString_
type JSObject a = JSRef (JSObject_ a)
-- type JSObject'  = JSRef (JSObject (Any *))

type JSArray a  = JSRef (JSArray_ a)



