{-# LANGUAGE FlexibleContexts, OverlappingInstances #-}
module Tests.Marshal (
  tests
) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import GHCJS.Marshal.Pure (PFromJSRef(..), PToJSRef(..))
import GHCJS.Marshal (FromJSRef(..), ToJSRef(..))
import Tests.QuickCheckUtils (eq)
import Test.QuickCheck.Monadic (run, monadicIO)
import Test.QuickCheck (once, Arbitrary(..), Property)
import Data.Int (Int32, Int16, Int8)
import Data.Word (Word32, Word16, Word8)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.JSString (JSString)

newtype TypeName a = TypeName String

pure_to_from_jsref' :: (PToJSRef a, PFromJSRef a, Eq a) => a -> Bool
pure_to_from_jsref' a = pFromJSRef (pToJSRef a) == a

pure_to_from_jsref :: (PToJSRef a, PFromJSRef a, Eq a) => TypeName a -> a -> Bool
pure_to_from_jsref _ = pure_to_from_jsref'

pure_to_from_jsref_maybe :: (PToJSRef a, PFromJSRef a, Eq a) => TypeName a -> Maybe a -> Bool
pure_to_from_jsref_maybe _ = pure_to_from_jsref'

to_from_jsref' :: (ToJSRef a, FromJSRef a, Eq a) => a -> Property
to_from_jsref' a = monadicIO $ do
    b <- run $ toJSRef a >>= fromJSRefUnchecked
    return $ b == a

to_from_jsref :: (ToJSRef a, FromJSRef a, Eq a) => TypeName a -> a -> Property
to_from_jsref _ = to_from_jsref'

to_from_jsref_maybe :: (ToJSRef a, FromJSRef a, Eq a) => TypeName a -> Maybe a -> Property
to_from_jsref_maybe _ = to_from_jsref'

to_from_jsref_list :: (ToJSRef a, FromJSRef a, Eq a) => TypeName a -> [a] -> Property
to_from_jsref_list _ = to_from_jsref'

to_from_jsref_list_maybe :: (ToJSRef a, FromJSRef a, Eq a) => TypeName a -> [Maybe a] -> Property
to_from_jsref_list_maybe _ = to_from_jsref'

to_from_jsref_list_list :: (ToJSRef a, FromJSRef a, Eq a) => TypeName a -> [[a]] -> Property
to_from_jsref_list_list _ = to_from_jsref'

to_from_jsref_maybe_list :: (ToJSRef a, FromJSRef a, Eq a) => TypeName a -> Maybe [a] -> Property
to_from_jsref_maybe_list _ = to_from_jsref'

pureMarshalTestGroup :: (PToJSRef a, PFromJSRef a, ToJSRef a, FromJSRef a, Eq a, Show a, Arbitrary a) => TypeName a -> Test
pureMarshalTestGroup t@(TypeName n) =
    testGroup n [
        testProperty "pure_to_from_jsref"       (pure_to_from_jsref t),
        testProperty "pure_to_from_jsref_maybe" (pure_to_from_jsref_maybe t),
        testProperty "to_from_jsref"            (to_from_jsref t),
        testProperty "to_from_jsref_maybe"      (to_from_jsref_maybe t),
        testProperty "to_from_jsref_list"       (to_from_jsref_list t),
        testProperty "to_from_jsref_list_maybe" (to_from_jsref_list_maybe t),
        testProperty "to_from_jsref_list_list"  (once $ to_from_jsref_list_list t),
        testProperty "to_from_jsref_maybe_list" (to_from_jsref_maybe_list t)
    ]

marshalTestGroup :: (ToJSRef a, FromJSRef a, Eq a, Show a, Arbitrary a) => TypeName a -> Test
marshalTestGroup t@(TypeName n) =
  testGroup n [testProperty "to_from_jsref" (to_from_jsref t)]

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary
    shrink = map T.pack . shrink . T.unpack

tests :: Test
tests =
  testGroup "Marshal" [
    pureMarshalTestGroup (TypeName "Bool"     :: TypeName Bool    ),
    pureMarshalTestGroup (TypeName "Int"      :: TypeName Int     ),
    pureMarshalTestGroup (TypeName "Int8"     :: TypeName Int8    ),
    pureMarshalTestGroup (TypeName "Int16"    :: TypeName Int16   ),
    pureMarshalTestGroup (TypeName "Int32"    :: TypeName Int32   ),
    pureMarshalTestGroup (TypeName "Word"     :: TypeName Word    ),
    pureMarshalTestGroup (TypeName "Word8"    :: TypeName Word8   ),
    pureMarshalTestGroup (TypeName "Word16"   :: TypeName Word16  ),
    pureMarshalTestGroup (TypeName "Word32"   :: TypeName Word32  ),
    pureMarshalTestGroup (TypeName "Float"    :: TypeName Float   ),
    pureMarshalTestGroup (TypeName "Double"   :: TypeName Double  ),
    pureMarshalTestGroup (TypeName "[Char]"   :: TypeName [Char]  ),
    pureMarshalTestGroup (TypeName "Text"     :: TypeName Text    ),
    pureMarshalTestGroup (TypeName "JSString" :: TypeName JSString)
  ]
