-- | QuickCheck properties for JSString, based on those from the text library.

{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings,
             ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fno-warn-missing-signatures #-}
module Tests.Properties
    (
      tests
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***), second)
import Data.Bits ((.&.))
import Data.Char (chr, isDigit, isHexDigit, isLower, isSpace, isUpper, ord)
-- import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (Monoid(..))
import Data.String (fromString)
-- import Data.Word (Word, Word8, Word16, Word32, Word64)
-- import Numeric (showEFloat, showFFloat, showGFloat, showHex)
import Prelude hiding (replicate)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property (Property(..))
-- import Text.Show.Functions ()
import qualified Control.Exception as Exception
import qualified Data.Bits as Bits (shiftL, shiftR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Word (Word, Word8, Word16, Word32, Word64)

import qualified System.IO as IO

import qualified Tests.SlowFunctions as Slow
import Tests.Properties.Numeric
import Tests.QuickCheckUtils
import Tests.Utils

import qualified Data.JSString as J
import qualified Data.JSString.Int as JI
import qualified Data.JSString.RealFloat as JR
import           Data.JSString.Internal.Search (indices)
import qualified Data.JSString.Internal.Fusion as S
import qualified Data.JSString.Internal.Fusion.Common as S

j_pack_unpack       = (J.unpack . J.pack) `eq` id
j_pack_unpack'      = (J.unpack' . J.pack) `eq` id
j_stream_unstream   = (S.unstream . S.stream) `eq` id
j_reverse_stream t  = (S.reverse . S.reverseStream) t == t
j_singleton c       = [c] == (J.unpack . J.singleton) c
j_singleton' c      = [c] == (J.unpack' . J.singleton) c

s_Eq s            = (s==)    `eq` ((S.streamList s==) . S.streamList)
    where _types = s :: String
sf_Eq p s =
    ((L.filter p s==) . L.filter p) `eq`
    (((S.filter p $ S.streamList s)==) . S.filter p . S.streamList)
j_Eq s            = (s==)    `eq` ((J.pack s==) . J.pack)
s_Ord s           = (compare s) `eq` (compare (S.streamList s) . S.streamList)
    where _types = s :: String
sf_Ord p s =
    ((compare $ L.filter p s) . L.filter p) `eq`
    (compare (S.filter p $ S.streamList s) . S.filter p . S.streamList)
j_Ord s           = (compare s) `eq` (compare (J.pack s) . J.pack)
j_Read            = id       `eq` (J.unpack . read . show)
j_Show            = show     `eq` (show . J.pack)
j_mappend s       = mappend s`eqP` (unpackS . mappend (J.pack s))
j_mconcat         = unsquare $
                    mconcat `eq` (unpackS . mconcat . L.map J.pack)
j_mempty          = mempty == (unpackS (mempty :: J.JSString))
j_IsString        = fromString  `eqP` (J.unpack . fromString)

s_cons x          = (x:)     `eqP` (unpackS . S.cons x)
s_cons_s x        = (x:)     `eqP` (unpackS . S.unstream . S.cons x)
sf_cons p x       = ((x:) . L.filter p) `eqP` (unpackS . S.cons x . S.filter p)
j_cons x          = (x:)     `eqP` (unpackS . J.cons x)
s_snoc x          = (++ [x]) `eqP` (unpackS . (flip S.snoc) x)
j_snoc x          = (++ [x]) `eqP` (unpackS . (flip J.snoc) x)
s_append s        = (s++)    `eqP` (unpackS . S.append (S.streamList s))
s_append_s s      = (s++)    `eqP`
                    (unpackS . S.unstream . S.append (S.streamList s))
sf_append p s     = (L.filter p s++) `eqP`
                    (unpackS . S.append (S.filter p $ S.streamList s))
j_append s        = (s++)    `eqP` (unpackS . J.append (packS s))

uncons (x:xs) = Just (x,xs)
uncons _      = Nothing

s_uncons          = uncons   `eqP` (fmap (second unpackS) . S.uncons)
sf_uncons p       = (uncons . L.filter p) `eqP`
                    (fmap (second unpackS) . S.uncons . S.filter p)
j_uncons          = uncons   `eqP` (fmap (second unpackS) . J.uncons)
s_head            = head   `eqP` S.head
sf_head p         = (head . L.filter p) `eqP` (S.head . S.filter p)
j_head            = head   `eqP` J.head
s_last            = last   `eqP` S.last
sf_last p         = (last . L.filter p) `eqP` (S.last . S.filter p)
j_last            = last   `eqP` J.last
s_tail            = tail   `eqP` (unpackS . S.tail)
s_tail_s          = tail   `eqP` (unpackS . S.unstream . S.tail)
sf_tail p         = (tail . L.filter p) `eqP` (unpackS . S.tail . S.filter p)
j_tail            = tail   `eqP` (unpackS . J.tail)
s_init            = init   `eqP` (unpackS . S.init)
s_init_s          = init   `eqP` (unpackS . S.unstream . S.init)
sf_init p         = (init . L.filter p) `eqP` (unpackS . S.init . S.filter p)
j_init            = init   `eqP` (unpackS . J.init)
s_null            = null   `eqP` S.null
sf_null p         = (null . L.filter p) `eqP` (S.null . S.filter p)
j_null            = null   `eqP` J.null
s_length          = length `eqP` S.length
sf_length p       = (length . L.filter p) `eqP` (S.length . S.filter p)
j_length          = length `eqP` J.length
j_compareLength t = (compare (J.length t)) `eq` J.compareLength t

s_map f           = map f  `eqP` (unpackS . S.map f)
s_map_s f         = map f  `eqP` (unpackS . S.unstream . S.map f)
sf_map p f        = (map f . L.filter p)  `eqP` (unpackS . S.map f . S.filter p)
j_map f           = map f  `eqP` (unpackS . J.map f)
s_intercalate c   = unsquare $
                    L.intercalate c `eq`
                    (unpackS . S.intercalate (packS c) . map packS)
j_intercalate c   = unsquare $
                    L.intercalate c `eq`
                    (unpackS . J.intercalate (packS c) . map packS)
s_intersperse c   = L.intersperse c `eqP`
                    (unpackS . S.intersperse c)
s_intersperse_s c = L.intersperse c `eqP`
                    (unpackS . S.unstream . S.intersperse c)
sf_intersperse p c= (L.intersperse c . L.filter p) `eqP`
                   (unpackS . S.intersperse c . S.filter p)
j_intersperse c   = unsquare $
                    L.intersperse c `eqP` (unpackS . J.intersperse c)
j_transpose       = unsquare $
                    L.transpose `eq` (map unpackS . J.transpose . map packS)
j_reverse         = L.reverse `eqP` (unpackS . J.reverse)
-- s_reverse_short n = L.reverse `eqP` (unpackS . S.reverse . shorten n . S.stream)

j_replace s d     = (L.intercalate d . splitOn s) `eqP`
                    (unpackS . J.replace (J.pack s) (J.pack d))

splitOn :: (Show a, Eq a) => [a] -> [a] -> [[a]]
splitOn pat src0
    | l == 0    = error "splitOn: empty"
    | otherwise = go src0
  where
    l           = length pat
    go src      = search 0 src
      where
        search _ [] = [src]
        search !n s@(_:s')
            | pat `L.isPrefixOf` s = take n src : go (drop l s)
            | otherwise            = search (n+1) s'

s_toCaseFold_length xs = S.length (S.toCaseFold s) >= length xs
    where s = S.streamList xs
sf_toCaseFold_length p xs =
    (S.length . S.toCaseFold . S.filter p $ s) >= (length . L.filter p $ xs)
    where s = S.streamList xs
j_toCaseFold_length t = J.length (J.toCaseFold t) >= J.length t
j_toLower_length t = J.length (J.toLower t) >= J.length t
j_toLower_lower t = p (J.toLower t) >= p t
    where p = J.length . J.filter isLower
j_toUpper_length t = J.length (J.toUpper t) >= J.length t
j_toUpper_upper t = p (J.toUpper t) >= p t
    where p = J.length . J.filter isUpper

justifyLeft k c xs  = xs ++ L.replicate (k - length xs) c
justifyRight m n xs = L.replicate (m - length xs) n ++ xs
center k c xs
    | len >= k  = xs
    | otherwise = L.replicate l c ++ xs ++ L.replicate r c
   where len = length xs
         d   = k - len
         r   = d `div` 2
         l   = d - r

s_justifyLeft k c = justifyLeft j c `eqP` (unpackS . S.justifyLeftI j c)
    where j = fromIntegral (k :: Word8)
s_justifyLeft_s k c = justifyLeft j c `eqP`
                      (unpackS . S.unstream . S.justifyLeftI j c)
    where j = fromIntegral (k :: Word8)
sf_justifyLeft p k c = (justifyLeft j c . L.filter p) `eqP`
                       (unpackS . S.justifyLeftI j c . S.filter p)
    where j = fromIntegral (k :: Word8)
j_justifyLeft k c = justifyLeft j c `eqP` (unpackS . J.justifyLeft j c)
    where j = fromIntegral (k :: Word8)
j_justifyRight k c = justifyRight j c `eqP` (unpackS . J.justifyRight j c)
    where j = fromIntegral (k :: Word8)

j_center k c = center j c `eqP` (unpackS . J.center j c)
    where j = fromIntegral (k :: Word8)

sf_foldl p f z    = (L.foldl f z . L.filter p) `eqP` (S.foldl f z . S.filter p)
    where _types  = f :: Char -> Char -> Char
j_foldl f z       = L.foldl f z  `eqP` (J.foldl f z)
    where _types  = f :: Char -> Char -> Char
sf_foldl' p f z   = (L.foldl' f z . L.filter p) `eqP`
                    (S.foldl' f z . S.filter p)
    where _types  = f :: Char -> Char -> Char
j_foldl' f z      = L.foldl' f z `eqP` J.foldl' f z
    where _types  = f :: Char -> Char -> Char
sf_foldl1 p f     = (L.foldl1 f . L.filter p) `eqP` (S.foldl1 f . S.filter p)
j_foldl1 f        = L.foldl1 f   `eqP` J.foldl1 f
sf_foldl1' p f    = (L.foldl1' f . L.filter p) `eqP` (S.foldl1' f . S.filter p)
j_foldl1' f       = L.foldl1' f  `eqP` J.foldl1' f
sf_foldr p f z    = (L.foldr f z . L.filter p) `eqP` (S.foldr f z . S.filter p)
    where _types  = f :: Char -> Char -> Char
j_foldr f z       = L.foldr f z  `eqP` J.foldr f z
    where _types  = f :: Char -> Char -> Char
sf_foldr1 p f     = unsquare $
                    (L.foldr1 f . L.filter p) `eqP` (S.foldr1 f . S.filter p)
j_foldr1 f        = L.foldr1 f   `eqP` J.foldr1 f

s_concat_s        = unsquare $
                    L.concat `eq` (unpackS . S.unstream . S.concat . map packS)
sf_concat p       = unsquare $
                    (L.concat . map (L.filter p)) `eq`
                    (unpackS . S.concat . map (S.filter p . packS))
j_concat          = unsquare $
                    L.concat `eq` (unpackS . J.concat . map packS)
sf_concatMap p f  = unsquare $ (L.concatMap f . L.filter p) `eqP`
                               (unpackS . S.concatMap (packS . f) . S.filter p)
j_concatMap f     = unsquare $
                    L.concatMap f `eqP` (unpackS . J.concatMap (packS . f))
sf_any q p        = (L.any p . L.filter q) `eqP` (S.any p . S.filter q)
j_any p           = L.any p       `eqP` J.any p
sf_all q p        = (L.all p . L.filter q) `eqP` (S.all p . S.filter q)
j_all p           = L.all p       `eqP` J.all p
sf_maximum p      = (L.maximum . L.filter p) `eqP` (S.maximum . S.filter p)
j_maximum         = L.maximum     `eqP` J.maximum
sf_minimum p      = (L.minimum . L.filter p) `eqP` (S.minimum . S.filter p)
j_minimum         = L.minimum     `eqP` J.minimum

sf_scanl p f z    = (L.scanl f z . L.filter p) `eqP`
                    (unpackS . S.scanl f z . S.filter p)
j_scanl f z       = L.scanl f z   `eqP` (unpackS . J.scanl f z)
j_scanl1 f        = L.scanl1 f    `eqP` (unpackS . J.scanl1 f)
j_scanr f z       = L.scanr f z   `eqP` (unpackS . J.scanr f z)
j_scanr1 f        = L.scanr1 f    `eqP` (unpackS . J.scanr1 f)

j_mapAccumL f z   = L.mapAccumL f z `eqP` (second unpackS . J.mapAccumL f z)
    where _types  = f :: Int -> Char -> (Int,Char)
j_mapAccumR f z   = L.mapAccumR f z `eqP` (second unpackS . J.mapAccumR f z)
    where _types  = f :: Int -> Char -> (Int,Char)

replicate n l = concat (L.replicate n l)

s_replicate n     = replicate m `eq`
                    (unpackS . S.replicateI (fromIntegral m) . packS)
    where m = fromIntegral (n :: Word8)
j_replicate n     = replicate m `eq` (unpackS . J.replicate m . packS)
    where m = fromIntegral (n :: Word8)

unf :: Int -> Char -> Maybe (Char, Char)
unf n c | fromEnum c * 100 > n = Nothing
        | otherwise            = Just (c, succ c)

j_unfoldr n       = L.unfoldr (unf m) `eq` (unpackS . J.unfoldr (unf m))
    where m = fromIntegral (n :: Word16)
j_unfoldrN n m    = (L.take i . L.unfoldr (unf j)) `eq`
                         (unpackS . J.unfoldrN i (unf j))
    where i = fromIntegral (n :: Word16)
          j = fromIntegral (m :: Word16)

unpack2 :: (Stringy s) => (s,s) -> (String,String)
unpack2 = unpackS *** unpackS

s_take n          = L.take n      `eqP` (unpackS . S.take n)
s_take_s m        = L.take n      `eqP` (unpackS . S.unstream . S.take n)
  where n = small m
sf_take p n       = (L.take n . L.filter p) `eqP`
                    (unpackS . S.take n . S.filter p)
j_take n          = L.take n      `eqP` (unpackS . J.take n)
j_takeEnd n       = (L.reverse . L.take n . L.reverse) `eqP`
                    (unpackS . J.takeEnd n)
s_drop n          = L.drop n      `eqP` (unpackS . S.drop n)
s_drop_s m        = L.drop n      `eqP` (unpackS . S.unstream . S.drop n)
  where n = small m
sf_drop p n       = (L.drop n . L.filter p) `eqP`
                    (unpackS . S.drop n . S.filter p)
j_drop n          = L.drop n      `eqP` (unpackS . J.drop n)
j_dropEnd n       = (L.reverse . L.drop n . L.reverse) `eqP`
                    (unpackS . J.dropEnd n)
s_take_drop m     = (L.take n . L.drop n) `eqP` (unpackS . S.take n . S.drop n)
  where n = small m
s_take_drop_s m   = (L.take n . L.drop n) `eqP`
                    (unpackS . S.unstream . S.take n . S.drop n)
  where n = small m
s_takeWhile p     = L.takeWhile p `eqP` (unpackS . S.takeWhile p)
s_takeWhile_s p   = L.takeWhile p `eqP` (unpackS . S.unstream . S.takeWhile p)
sf_takeWhile q p  = (L.takeWhile p . L.filter q) `eqP`
                    (unpackS . S.takeWhile p . S.filter q)
j_takeWhile p     = L.takeWhile p `eqP` (unpackS . J.takeWhile p)
s_dropWhile p     = L.dropWhile p `eqP` (unpackS . S.dropWhile p)
s_dropWhile_s p   = L.dropWhile p `eqP` (unpackS . S.unstream . S.dropWhile p)
sf_dropWhile q p  = (L.dropWhile p . L.filter q) `eqP`
                    (unpackS . S.dropWhile p . S.filter q)
j_dropWhile p     = L.dropWhile p `eqP` (unpackS . J.dropWhile p)
j_dropWhileEnd p  = (L.reverse . L.dropWhile p . L.reverse) `eqP`
                    (unpackS . J.dropWhileEnd p)
j_dropAround p    = (L.dropWhile p . L.reverse . L.dropWhile p . L.reverse)
                    `eqP` (unpackS . J.dropAround p)
j_stripStart      = J.dropWhile isSpace `eq` J.stripStart
j_stripEnd        = J.dropWhileEnd isSpace `eq` J.stripEnd
j_strip           = J.dropAround isSpace `eq` J.strip
j_splitAt n       = L.splitAt n   `eqP` (unpack2 . J.splitAt n)
j_span p          = L.span p      `eqP` (unpack2 . J.span p)

j_breakOn_id s      = squid `eq` (uncurry J.append . J.breakOn s)
  where squid t | J.null s  = error "empty"
                | otherwise = t
j_breakOn_start (NotEmpty s) t =
    let (k,m) = J.breakOn s t
    in k `J.isPrefixOf` t && (J.null m || s `J.isPrefixOf` m)
j_breakOnEnd_end (NotEmpty s) t =
    let (m,k) = J.breakOnEnd s t
    in k `J.isSuffixOf` t && (J.null m || s `J.isSuffixOf` m)
j_break p       = L.break p     `eqP` (unpack2 . J.break p)
j_group           = L.group       `eqP` (map unpackS . J.group)
j_groupBy p       = L.groupBy p   `eqP` (map unpackS . J.groupBy p)
j_inits           = L.inits       `eqP` (map unpackS . J.inits)
j_tails           = L.tails       `eqP` (map unpackS . J.tails)
j_findAppendId = unsquare $ \(NotEmpty s) ts ->
    let t = J.intercalate s ts
    in all (==t) $ map (uncurry J.append) (J.breakOnAll s t)

j_findContains = unsquare $ \(NotEmpty s) ->
    all (J.isPrefixOf s . snd) . J.breakOnAll s . J.intercalate s
j_findContains' = unsquare $ \(NotEmpty s) ->
    all (J.isPrefixOf s . snd) . J.breakOnAll' s . J.intercalate s

j_findCount s     = (L.length . J.breakOnAll s) `eq` J.count s
j_findCount' s    = (L.length . J.breakOnAll' s) `eq` J.count s

j_splitOn_split s  = unsquare $
                     (J.splitOn s `eq` Slow.splitOn s) . J.intercalate s
j_splitOn'_split s  = unsquare $
                     (J.splitOn' s `eq` Slow.splitOn s) . J.intercalate s
j_splitOn_i (NotEmpty t)  = id `eq` (J.intercalate t . J.splitOn t)
j_splitOn'_i (NotEmpty t)  = id `eq` (J.intercalate t . J.splitOn' t)

j_split p       = split p `eqP` (map unpackS . J.split p)
j_split_count c = (L.length . J.split (==c)) `eq`
                  ((1+) . J.count (J.singleton c))
j_split_splitOn c = J.split (==c) `eq` J.splitOn (J.singleton c)
j_split_splitOn' c = J.split (==c) `eq` J.splitOn' (J.singleton c)

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] =  [[]]
split p xs = loop xs
    where loop s | null s'   = [l]
                 | otherwise = l : loop (tail s')
              where (l, s') = break p s

j_chunksOf_same_lengths k = all ((==k) . J.length) . ini . J.chunksOf k
  where ini [] = []
        ini xs = init xs
j_chunksOf_same_lengths' k = all ((==k) . J.length) . ini . J.chunksOf' k
  where ini [] = []
        ini xs = init xs

j_chunksOf_length k t = len == J.length t || (k <= 0 && len == 0)
  where len = L.sum . L.map J.length $ J.chunksOf k t
j_chunksOf_length' k t = len == J.length t || (k <= 0 && len == 0)
  where len = L.sum . L.map J.length $ J.chunksOf' k t

j_lines           = L.lines       `eqP` (map unpackS . J.lines)
j_lines'          = L.lines       `eqP` (map unpackS . J.lines')

j_words           = L.words       `eqP` (map unpackS . J.words)
j_words'          = L.words       `eqP` (map unpackS . J.words')

j_unlines         = unsquare $
                    L.unlines `eq` (unpackS . J.unlines . map packS)
j_unwords         = unsquare $
                    L.unwords `eq` (unpackS . J.unwords . map packS)

s_isPrefixOf s    = L.isPrefixOf s `eqP`
                    (S.isPrefixOf (S.stream $ packS s) . S.stream)
sf_isPrefixOf p s = (L.isPrefixOf s . L.filter p) `eqP`
                    (S.isPrefixOf (S.stream $ packS s) . S.filter p . S.stream)
j_isPrefixOf s    = L.isPrefixOf s`eqP` J.isPrefixOf (packS s)
j_isSuffixOf s    = L.isSuffixOf s`eqP` J.isSuffixOf (packS s)
j_isInfixOf s     = L.isInfixOf s `eqP` J.isInfixOf (packS s)

j_stripPrefix s      = (fmap packS . L.stripPrefix s) `eqP` J.stripPrefix (packS s)

stripSuffix p t = reverse `fmap` L.stripPrefix (reverse p) (reverse t)

j_stripSuffix s      = (fmap packS . stripSuffix s) `eqP` J.stripSuffix (packS s)

commonPrefixes a0@(_:_) b0@(_:_) = Just (go a0 b0 [])
    where go (a:as) (b:bs) ps
              | a == b = go as bs (a:ps)
          go as bs ps  = (reverse ps,as,bs)
commonPrefixes _ _ = Nothing

j_commonPrefixes a b (NonEmpty p)
    = commonPrefixes pa pb ==
      repack `fmap` J.commonPrefixes (packS pa) (packS pb)
  where repack (x,y,z) = (unpackS x,unpackS y,unpackS z)
        pa = p ++ a
        pb = p ++ b

sf_elem p c       = (L.elem c . L.filter p) `eqP` (S.elem c . S.filter p)
sf_filter q p     = (L.filter p . L.filter q) `eqP`
                    (unpackS . S.filter p . S.filter q)
j_filter p        = L.filter p    `eqP` (unpackS . J.filter p)
sf_findBy q p     = (L.find p . L.filter q) `eqP` (S.findBy p . S.filter q)
j_find p          = L.find p      `eqP` J.find p
j_partition p     = L.partition p `eqP` (unpack2 . J.partition p)

sf_index p s      = forAll (choose (-l,l*2))
                    ((L.filter p s L.!!) `eq` S.index (S.filter p $ packS s))
    where l = L.length s
j_index s         = forAll (choose (-l,l*2)) ((s L.!!) `eq` J.index (packS s))
    where l = L.length s

j_findIndex p     = L.findIndex p `eqP` J.findIndex p
j_count (NotEmpty t)  = (subtract 1 . L.length . J.splitOn t) `eq` J.count t
j_zip s           = L.zip s `eqP` J.zip (packS s)
sf_zipWith p c s  = (L.zipWith c (L.filter p s) . L.filter p) `eqP`
                    (unpackS . S.zipWith c (S.filter p $ packS s) . S.filter p)
j_zipWith c s     = L.zipWith c s `eqP` (unpackS . J.zipWith c (packS s))

j_indices  (NotEmpty s) = Slow.indices s `eq` indices s
j_indices_occurs = unsquare $ \(NotEmpty t) ts ->
    let s = J.intercalate t ts
    in Slow.indices t s == indices t s

-- Reading.
{-
j_decimal (n::Int) s =
    J.signed J.decimal (J.pack (show n) `J.append` t) == Right (n,t)
    where t = J.dropWhile isDigit s
j_hexadecimal m s ox =
    J.hexadecimal (J.concat [p, J.pack (showHex n ""), t]) == Right (n,t)
    where t = J.dropWhile isHexDigit s
          p = if ox then "0x" else ""
          n = getPositive m :: Int

isFloaty c = c `elem` "+-.0123456789eE"

j_read_rational p tol (n::Double) s =
    case p (J.pack (show n) `J.append` t) of
      Left _err     -> False
      Right (n',t') -> t == t' && abs (n-n') <= tol
    where t = J.dropWhile isFloaty s

j_double = j_read_rational J.double 1e-13
j_rational = j_read_rational J.rational 1e-16
-}
-- Input and output.
{-
t_put_get = write_read T.unlines T.filter put get
  where put h = withRedirect h IO.stdout . T.putStr
        get h = withRedirect h IO.stdin T.getContents
tl_put_get = write_read TL.unlines TL.filter put get
  where put h = withRedirect h IO.stdout . TL.putStr
        get h = withRedirect h IO.stdin TL.getContents
t_write_read = write_read T.unlines T.filter T.hPutStr T.hGetContents
tl_write_read = write_read TL.unlines TL.filter TL.hPutStr TL.hGetContents

t_write_read_line e m b t = write_read head T.filter T.hPutStrLn
                            T.hGetLine e m b [t]
tl_write_read_line e m b t = write_read head TL.filter TL.hPutStrLn
                             TL.hGetLine e m b [t]
-}
-- Low-level.
{-
j_dropWord16 m t = dropWord16 m t `J.isSuffixOf` t
j_takeWord16 m t = takeWord16 m t `J.isPrefixOf` t
j_take_drop_16 m t = J.append (takeWord16 n t) (dropWord16 n t) == t
  where n = small m
j_use_from t = monadicIO $ assert . (==t) =<< run (useAsPtr t fromPtr)
-}
-- Regression tests.
s_filter_eq s = S.filter p t == S.streamList (filter p s)
    where p = (/= S.last t)
          t = S.streamList s

tests :: Test
tests =
  testGroup "Properties" []
