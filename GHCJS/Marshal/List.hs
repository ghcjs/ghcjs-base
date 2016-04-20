{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module implements instances of 'ToJS' and 'FromJS' for lists,
-- specializing the instance for @[Char]@ ('String') so that it is
-- efficiently converted to and from a 'JSString'.
--
-- It safely uses @-XUndecidableInstances@ and a closed type family to
-- accomplish this, and the result is that
--
-- > toJS [True] :: JSArray
--
-- Whereas
--
-- > toJS "Hi!" :: JSString
--
-- Since this uses a closed type family, no additional specializations
-- can be added for lists. This is why the methods for the `ListToJSVal`
-- and `ListFromJSVal` classes aren't exported.
module GHCJS.Marshal.List
  ( ListToJSVal
  , ListFromJSVal
  , JSListType
  ) where

import           Control.Monad
import           GHCJS.Marshal.Pure
import           GHCJS.Marshal.Internal
import           GHCJS.Types
import qualified GHCJS.Prim as Prim
import           JavaScript.Array (JSArray)

instance ListToJSVal a => ToJSVal [a] where
  toJSVal = listToJSVal

instance ListFromJSVal a => FromJSVal [a] where
  fromJSVal = listFromJSVal

-- | This implements conversion of a list of values to a javascript
-- reference. The type parameter is the type of the list element. If
-- it's 'Char', then the result is a 'JSString'. Otherwise, it's a
-- 'JSArray'.
class ListToJSVal a where
  listToJSVal :: [a] -> IO JSVal

instance {-# OVERLAPPABLE #-} ToJSVal a => ListToJSVal a where
  listToJSVal = Prim.toJSArray <=< mapM toJSVal

instance {-# OVERLAPPING #-} ListToJSVal Char where
  listToJSVal = toJSVal_pure

-- This implements conversion of a javascript reference to a list of
-- values. The type parameter is the type of the list element. If it's
-- 'Char', then it expects a 'JSString'. Otherwise, it expects a
-- 'JSArray'.
class ListFromJSVal a where
  listFromJSVal :: JSVal -> IO (Maybe [a])

instance {-# OVERLAPPABLE #-} FromJSVal a => ListFromJSVal a where
  listFromJSVal = fmap sequence . (mapM fromJSVal <=< Prim.fromJSArray) -- fixme should check that it's an array

instance {-# OVERLAPPING #-} ListFromJSVal Char where
  listFromJSVal = fromJSVal_pure

type family JSListType a where
  JSListType Char = JSString
  -- FIXME: this should really have the list type.
  JSListType a = JSArray
