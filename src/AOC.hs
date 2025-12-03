{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_HADDOCK prune, ignore-exports #-}

module AOC (module Prelude, module AOC, module Text.Megaparsec, module Text.Megaparsec.Char, module Data.Vector, module Data.Char, module Data.List, module Data.List.Split, module Data.List.Extra, module Data.Hashable, module Data.Maybe, module Data.Either, module Data.Bool, module Control.Monad, module Control.Arrow, module Data.Ord) where

import Control.Arrow
-- hiding (interact)

import Control.Concurrent (modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Exception (ArithException (..))
import Control.Monad
import Data.Bits
import Data.Bool
import Data.Char
import Data.Either
import Data.Function (on)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable (hashWithSalt), hash)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List hiding (nub)
import Data.List.Extra hiding (chunksOf, enumerate, linesBy, lower, merge, nub, split, splitOn, upper, wordsBy)
import Data.List.Split hiding (chunk, endBy, oneOf, sepBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.Merge.Strict
import Data.Map.Strict qualified as MS
import Data.Maybe
import Data.Ord
import Data.Set qualified as S
import Data.Tuple
import Data.Vector (Vector, imap)
import Data.Vector qualified as V
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed qualified as U
import Data.Void
import Numeric
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec hiding (chunk, parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (symbol)
import Prelude
import Prelude qualified

type Input = String

type Error = String

type Parser a = Parsec Void Input a

type VGrid a = Vector (Vector a)

parse :: Parser a -> Input -> Either Error a
parse parser input =
  case runParser parser "" input of
    Left err -> Left $ errorBundlePretty err
    Right x -> Right x

parseLines :: Parser a -> Input -> Either Error [a]
parseLines p = parse p'
  where
    p' = do
      ps <- p `sepEndBy` eol
      eof
      return ps

slice from to xs = take (to - from + 1) (drop from xs)

ltov2 :: [[a]] -> Vector (Vector a)
ltov2 = V.fromList . map V.fromList

v2tom :: VGrid a -> M.Map (Int, Int) a
v2tom grid = M.fromList [((r, c), grid @ (r, c)) | r <- [1 .. n], c <- [1 .. m]]
  where
    n = length grid
    m = length (grid V.! 0)

(@) :: VGrid a -> (Int, Int) -> a
grid @ (r, c) = grid V.! r V.! c

l2tom :: [[a]] -> M.Map (Int, Int) a
l2tom ll = (M.fromList . zip coords . concat) ll
  where
    coords = (,) <$> [0 .. n] <*> [0 .. m]
    n = length ll - 1
    m = length (head ll) - 1

-- | Returns the 4 points orthogonally adjacent to the given point.
neighbours4 :: (Num a, Num b) => (a, b) -> [(a, b)]
neighbours4 (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- | Returns the 8 points orthogonally or diagonally adjacent to the given point.
neighbours8 :: (Eq a, Eq b, Num a, Num b) => (a, b) -> [(a, b)]
neighbours8 (x, y) = [(x + p, y + q) | p <- [-1, 0, 1], q <- [-1, 0, 1], p /= 0 || q /= 0]

-- | Returns the 6 points orthogonally adjacent to the given point in 3D space.
neighbours6 :: (Num a, Num b, Num c) => (a, b, c) -> [(a, b, c)]
neighbours6 (x, y, z) = [(x + 1, y, z), (x, y + 1, z), (x, y, z + 1), (x - 1, y, z), (x, y - 1, z), (x, y, z - 1)]

-- | Returns the 26 points orthogonally or diagonally adjacent to the given point in 3D space.
neighbours26 :: (Eq a, Eq b, Eq c, Num a, Num b, Num c) => (a, b, c) -> [(a, b, c)]
neighbours26 (x, y, z) = [(x + p, y + q, z + r) | p <- [-1, 0, 1], q <- [-1, 0, 1], r <- [-1, 0, 1], p /= 0 || q /= 0 || r /= 0]

-- | Returns the Taxicab/Manhattan distance between two points in 2D space.
taxicab2 :: (Num a) => (a, a) -> (a, a) -> a
taxicab2 (a, b) (c, d) = abs (a - c) + abs (b - d)

-- | Returns the Taxicab/Manhattan distance between two points in 3D space.
taxicab3 :: (Num a) => (a, a, a) -> (a, a, a) -> a
taxicab3 (a, b, c) (d, e, f) = abs (a - d) + abs (b - e) + abs (c - f)

-- | Returns the Taxicab/Manhattan distance between two points in n dimensions, where both points are lists of length n.
taxicab :: (Num a) => [a] -> [a] -> a
taxicab as bs = sum $ zipWith (\x y -> abs (x - y)) as bs

-- $cat2
-- The following functions (beginning with "enumerate") operate on a grid of characters as a string with a newline after each row (as seen in several Advent of Code puzzle inputs).

-- | Converts a grid to a list of pairs @((x,y),c)@ representing xy coordinates and the character at that location.
enumerate' :: (Num y, Num x) => String -> [((x, y), Char)]
enumerate' s =
  let ss = lines s
      ys = zipWith (\n l -> map (n,) l) (iterate (+ 1) 0) ss
      xs = map (zipWith (\x (y, c) -> ((x, y), c)) (iterate (+ 1) 0)) ys
   in concat xs

-- | Enumerates a grid along with reading the characters (usually as integers), and returns a list of pairs.
enumerateRead' :: (Read c, Num y, Num x) => String -> [((x, y), c)]
enumerateRead' = map (\((x, y), c) -> ((x, y), read [c])) . enumerate'

-- | Converts a grid to a list of triples @(x,y,c)@ representing xy coordinates and the character at that location.
enumerate :: (Num y, Num x) => String -> [(x, y, Char)]
enumerate = map (\((x, y), c) -> (x, y, c)) . enumerate'

-- | Enumerates a grid along with reading the characters (usually as integers), and returns a list of triples.
enumerateRead :: (Read c, Num y, Num x) => String -> [(x, y, c)]
enumerateRead = map (\((x, y), c) -> (x, y, read [c])) . enumerate'

-- | Enumerates a grid and stores it in a @HashMap@ where points are mapped to the character at that location.
enumerateHM :: (Num x, Num y, Enum x, Enum y, Hashable x, Hashable y) => String -> HM.HashMap (x, y) Char
enumerateHM = HM.fromList . enumerate'

-- | Enumerates a grid and stores it in a @HashMap@ along with reading the characters (usually as integers).
enumerateReadHM :: (Num x, Num y, Enum x, Enum y, Hashable x, Hashable y, Read c) => String -> HM.HashMap (x, y) c
enumerateReadHM = HM.fromList . map (\((x, y), c) -> ((x, y), read [c])) . enumerate'

-- | Returns a list of points on a grid for which a certain condition is met.
enumerateFilter :: (Num y, Num x) => (Char -> Bool) -> String -> [(x, y)]
enumerateFilter f = map fst . filter (f . snd) . enumerate'

-- | Returns a set of points on a grid for which a certain condition is met.
enumerateFilterSet :: (Ord x, Ord y, Num y, Num x) => (Char -> Bool) -> String -> S.Set (x, y)
enumerateFilterSet f = S.fromList . enumerateFilter f

-- | Returns all the integers in a string (including negative signs).
numbers :: (Num a, Read a) => [Char] -> [a]
numbers = map read . filter (isDigit . head) . groupBy ((==) `on` isDigit)
  where
    isDigit = (`elem` "1234567890-")

-- | Returns all the integers in a string (excluding negative signs).
numbers' :: (Num a, Read a) => [Char] -> [a]
numbers' = map read . filter (isDigit . head) . groupBy ((==) `on` isDigit)
  where
    isDigit = (`elem` "1234567890")

floodFill' :: (Ord a) => (a -> [a]) -> S.Set a -> [a] -> S.Set a -> S.Set a
floodFill' neighbours finished (f : frontier) blocks = floodFill' neighbours (S.insert f finished) (frontier ++ filtered) blocks
  where
    filtered = filter (\n -> n `S.notMember` finished && n `notElem` frontier && n `S.notMember` blocks) $ neighbours f
floodFill' _ finished [] _ = finished

floodFillWith' :: (Ord a) => (a -> a -> Bool) -> (a -> [a]) -> S.Set a -> [a] -> S.Set a
floodFillWith' cond neighbours finished (f : frontier) = floodFillWith' cond neighbours (S.insert f finished) (frontier ++ filtered)
  where
    filtered = filter (\n -> n `S.notMember` finished && n `notElem` frontier && cond f n) $ neighbours f
floodFillWith' _ _ finished [] = finished

-- | Applies a flood fill algorithm given a function to generate a point's neighbours, a starting set of points, and a set of points to avoid. Returns a set of all points covered.
floodFill ::
  (Ord a) =>
  -- | Neighbour function
  (a -> [a]) ->
  -- | Initial set of points
  S.Set a ->
  -- | Set of points to avoid
  S.Set a ->
  S.Set a
floodFill neighbours frontier = floodFill' neighbours S.empty (S.toList frontier)

-- | Applies a flood fill algorithm given a function to generate a point's neighbours, a condition that filters out points generated by said function, and a starting set of points. Returns a set of all points covered.
-- The condition is of the form @a -> a -> Bool@, which returns @True@ if the second point is a valid neighbour of the first point and @False@ otherwise.
floodFillWith ::
  (Ord a) =>
  -- | Condition
  (a -> a -> Bool) ->
  -- | Neighbour function
  (a -> [a]) ->
  -- | Initial set of points
  S.Set a ->
  S.Set a
floodFillWith cond neighbours frontier = floodFillWith' cond neighbours S.empty (S.toList frontier)

-- | Generates a list of all possible lists of length n by taking elements from the provided list of length l.
-- Relative order is maintained, and the length of the returned list is \(_{n}C_{l}\).
choose :: (Num n, Ord n) => n -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x : xs)
  | n > fromIntegral (length (x : xs)) = []
  | otherwise = map (x :) (choose (n - 1) xs) ++ choose n xs

-- | Generates a list of all possible lists of length n by taking elements from the provided list of length l.
-- The length of the returned list is \(_{n}P_{l}\).
permute :: (Num n, Ord n) => n -> [a] -> [[a]]
permute n = concatMap permutations . choose n

-- | Takes every nth element from a list xs, starting from @xs !! (n-1)@.
takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs = let (a, b) = splitAt n xs in if length a < n then [] else last a : takeEvery n b

-- | Splits a list into sublists of size n. The length of the last sublist may be less than n.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (a, b) = splitAt n xs in a : chunk n b

-- | Gets the nth element of an infinite list, assuming that each element in the list can be generated using the previous element, for example, a list generated with @iterate@.
extrapolate :: (Integral b, Ord a) => b -> [a] -> a
extrapolate n ls = let (o, p) = helper 0 S.empty ls in ls `genericIndex` (((n - o) `mod` p) + o)
  where
    helper k finished (l : ls')
      | S.null matches = helper (k + 1) (S.insert (k, l) finished) ls'
      | otherwise = let o = fst $ S.elemAt 0 matches in (o, k - o)
      where
        matches = S.filter ((== l) . snd) finished

-- | Converts a set of points @(x,y)@ to a string composed of @'#'@ and @' '@. This function is useful when displaying puzzle answers formed by a grid of points.
-- Up to translation of points, @prettyPrintSet . enumerateFilterSet (=='#') = id@.
prettyPrintSet :: (Enum b, Enum a, Ord a, Ord b) => S.Set (a, b) -> String
prettyPrintSet points = unlines [[if (x, y) `S.member` points then '#' else ' ' | x <- [xmin .. xmax]] | y <- reverse [ymin .. ymax]]
  where
    xs = S.map fst points
    ys = S.map snd points
    (xmin, xmax, ymin, ymax) = (minimum xs, maximum xs, minimum ys, maximum ys)

-- | Same as @prettyPrintSet@, but displays points at double width to improve readability.
prettyPrintSetWide :: (Enum b, Enum a, Ord a, Ord b) => S.Set (a, b) -> String
prettyPrintSetWide = foldr (\c acc -> if c /= '\n' then c : c : acc else c : acc) [] . prettyPrintSet

-- | Converts a @HashMap@ of points @(x,y)@ and characters @c@ to a string with the corresponding character at each point. This function is useful when displaying puzzle answers formed by a grid of points.
-- Up to translation of points, @prettyPrintHM . enumerateHM = id@.
prettyPrintHM :: (Enum b, Enum a, Hashable a, Hashable b, Ord a, Ord b) => HM.HashMap (a, b) Char -> String
prettyPrintHM points = unlines [[HM.lookupDefault ' ' (x, y) points | x <- [xmin .. xmax]] | y <- reverse [ymin .. ymax]]
  where
    xs = map fst $ HM.keys points
    ys = map snd $ HM.keys points
    (xmin, xmax, ymin, ymax) = (minimum xs, maximum xs, minimum ys, maximum ys)

-- | Same as @prettyPrintHM@, but displays points at double width to improve readability.
prettyPrintHMWide :: (Enum b, Enum a, Hashable a, Hashable b, Ord a, Ord b) => HM.HashMap (a, b) Char -> String
prettyPrintHMWide = foldr (\c acc -> if c /= '\n' then c : c : acc else c : acc) [] . prettyPrintHM

memo2 :: (Hashable a, Hashable b) => (a -> b -> c) -> (a -> b -> c)
memo2 f = unsafePerformIO $ do
  v <- newMVar HM.empty
  let f' a b = unsafePerformIO $ do
        m <- readMVar v
        case HM.lookup (a, b) m of
          Nothing -> do let { r = f a b }; modifyMVar_ v (return . HM.insert (a, b) r); return r
          Just r -> return r
  return f'

memo3 :: (Hashable a, Hashable b, Hashable c) => (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = unsafePerformIO $ do
  v <- newMVar HM.empty
  let f' a b c = unsafePerformIO $ do
        m <- readMVar v
        case HM.lookup (a, b, c) m of
          Nothing -> do let { r = f a b c }; modifyMVar_ v (return . HM.insert (a, b, c) r); return r
          Just r -> return r
  return f'

memo4 :: (Hashable a, Hashable b, Hashable c, Hashable d) => (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> e)
memo4 f = unsafePerformIO $ do
  v <- newMVar HM.empty
  let f' a b c d = unsafePerformIO $ do
        m <- readMVar v
        case HM.lookup (a, b, c, d) m of
          Nothing -> do let { r = f a b c d }; modifyMVar_ v (return . HM.insert (a, b, c, d) r); return r
          Just r -> return r
  return f'

-- | Generates a range with @[x..y]@, but reverses the list instead of returning an empty range if x > y.
range :: (Ord a, Enum a) => a -> a -> [a]
range x y = if y < x then [x, pred x .. y] else [x .. y]

-- | Takes (a,b) and (c,d) as arguments and returns the intersection of the ranges [a..b] and [c..d] as another pair if it is not empty.
rangeIntersect :: (Ord b) => (b, b) -> (b, b) -> Maybe (b, b)
rangeIntersect (a, b) (c, d)
  | b < c || a > d = Nothing
  | otherwise = Just (max a c, min b d)

-- | Converts a list of booleans (parsed as a binary number) to an integer.
binToDec :: (Num a) => [Bool] -> a
binToDec = sum . zipWith (*) (map (2 ^) [0 ..]) . map (fromIntegral . fromEnum) . reverse
