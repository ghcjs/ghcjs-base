module Tests.Buffer (
  tests
) where

import Control.Exception (ErrorCall(..), catch)
import Control.Monad (forM_)
import Data.Primitive.ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, ioProperty, (==>))
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), assertFailure)

import GHCJS.Buffer

byteLengthWorks :: Int -> Property
byteLengthWorks i = (i >= 0) ==> (ioProperty $ do
  buf <- create i
  return (i == byteLength buf))

fromByteStringNoOffset :: IO ()
fromByteStringNoOffset = do
  let bs = BS8.pack ['a'..'z']
      (buf, offset, len) = fromByteString bs
  0 @=? offset
  26 @=? len
  26 @=? byteLength buf
  let ba = toByteArray buf
  forM_ [0..25] $ \i -> do
    BS.index bs i @=? indexByteArray ba i

fromByteStringWithOffset :: IO ()
fromByteStringWithOffset = do
  let bs = BS8.pack ['a'..'z']
      sliced = BS.take 10 . BS.drop 5 $ bs
      (buf, offset, len) = fromByteString sliced
  5 @=? offset
  10 @=? len
  26 @=? byteLength buf
  let ba = toByteArray buf
  forM_ [0..25] $ \i -> do
    BS.index bs i @=? indexByteArray ba i

atoz :: IO ByteArray
atoz = do
  ba <- newByteArray 26
  forM_ [0..25] $ \i -> writeByteArray ba i (fromIntegral i + 65 :: Word8)
  unsafeFreezeByteArray ba

wrapIntoByteString :: IO ()
wrapIntoByteString = do
  buf <- fromByteArray `fmap` atoz
  BS8.pack ['A'..'Z'] @=? toByteString 0 Nothing buf
  BS8.pack ['K'..'Z'] @=? toByteString 10 Nothing buf
  BS8.pack ['K'..'O'] @=? toByteString 10 (Just 5) buf

toByteStringOffset :: IO ()
toByteStringOffset = do
  buf <- fromByteArray `fmap` atoz
  tst (toByteString (-1) Nothing buf) "toByteString: negative offset"
  tst (toByteString 100 Nothing buf) "toByteString: offset past end of buffer"
  tst (toByteString 0 (Just (-1)) buf) "toByteString: negative length"
  tst (toByteString 0 (Just (-1)) buf) "toByteString: negative length"
  tst (toByteString 0 (Just 27) buf) "toByteString: length past end of buffer"
  tst (toByteString 20 (Just 7) buf) "toByteString: length past end of buffer"
  where tst expr expected = ign expr expected `catch` \(ErrorCall msg) -> expected @=? msg
        ign expr expected = seq expr $ assertFailure ("Expected error " ++ show expected ++ " but got nothing")

tests :: Test
tests =
  testGroup "Buffer" [
    testProperty "byte length works" byteLengthWorks,
    testCase "fromByteString - no offset" fromByteStringNoOffset,
    testCase "fromByteString - offset" fromByteStringWithOffset,
    testCase "toByteString" wrapIntoByteString,
    testCase "toByteString - offset" toByteStringOffset
    ]
