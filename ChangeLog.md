# Revision history for ghcjs-base

## 0.8.1.0

* `GHCJS.Int53`: `Int53` and `Word53`, integers a JavaScript Number holds
  exactly (up to 2^53 - 1), with checked (`toInt53`), saturating
  (`clampInt53`) and literal (`int53`) conversions and no `Num` instance.
  jsaddle has the same module for native GHC.
* `PToJSVal`, `PFromJSVal`, `ToJSVal` and `FromJSVal` instances for
  `Object` and `SomeJSArray` (`JSArray`, `MutableJSArray`) in
  `GHCJS.Marshal.Internal`; `fromJSVal` is `Nothing` for `null` and
  `undefined`.  Packages with their own orphan instances for these types
  need to drop them.
