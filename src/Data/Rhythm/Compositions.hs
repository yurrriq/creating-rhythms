-- |
-- Module      : Data.Rhythm.Compositions
-- Description : Combinatorial compositions
-- Copyright   : (c) Eric Bailey, 2024-2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- [Combinatorial compositions](https://mathworld.wolfram.com/Composition.html),
-- i.e., [partitions]("Data.Rhythm.Partitions") in which order is significant.
module Data.Rhythm.Compositions
  ( Composition,
    compositions,
    compositionsAllowed,
    compositionsLength,
    compositionsLengthAllowed,
    randomComposition,
    randomCompositionLength,
  )
where

import Data.Bool (bool)
import Math.Combinat.Compositions (Composition, compositions1)
import System.Random (randomIO)

-- | All positive compositions of a given number.
--
-- >>> compositions 4
-- [[4],[1,3],[2,2],[3,1],[1,1,2],[1,2,1],[2,1,1],[1,1,1,1]]
compositions :: (Integral a) => a -> [Composition]
compositions n = concatMap (`compositions1` n) [1 .. n]

-- | All positive compositions with allowed parts.
--
-- >>> compositionsAllowed [1,2] 4
-- [[2,2],[1,1,2],[1,2,1],[2,1,1],[1,1,1,1]]
compositionsAllowed :: (Integral a) => [Int] -> a -> [Composition]
compositionsAllowed allowed = filter (all (`elem` allowed)) . compositions

-- | Positive compositions of a given length.
--
-- >>> compositionsLength 2 5
-- [[1,4],[2,3],[3,2],[4,1]]
--
-- The number of positive compositions of \(n\) into \(k\) parts is given by the
-- following formula.
--
-- \[
--   \begin{align*}
--     C_k(n) &= \binom{n - 1}{k - 1} \\
--     &= \frac{(n-1)!}{(k-1)!(n-k)!}
--   \end{align*}
-- \]
--
-- >>> let _C k n = toInteger (length (compositionsLength k n))
-- >>> let fact n = product [1 .. n]
-- >>> _C 2 5 == fact (5 - 1) `div` (fact (2 - 1) * fact (5 - 2))
-- True
compositionsLength :: Int -> Int -> [Composition]
compositionsLength = compositions1

-- | Positive compositions of a given length with allowed parts.
--
-- >>> compositionsLengthAllowed 2 [2,3] 5
-- [[2,3],[3,2]]
--
-- >>> filter (all (`elem` [2,3])) (compositionsLength 2 5)
-- [[2,3],[3,2]]
compositionsLengthAllowed :: Int -> [Int] -> Int -> [Composition]
compositionsLengthAllowed len allowed =
  filter (all (`elem` allowed)) . compositionsLength len

-- | Generate a random positive composition of a given number.
--
-- >>> sum <$> randomComposition 13
-- 13
randomComposition :: Int -> IO Composition
randomComposition n = go 1 (1, [])
  where
    go i (p, acc)
      | i == n = pure (p : acc)
      | otherwise = go (i + 1) . bool (1, p : acc) (p + 1, acc) =<< randomIO

-- | Generate a random positive composition of a given length.
--
-- >>> sum <$> randomCompositionLength 3 33
-- 33
randomCompositionLength :: Int -> Int -> IO Composition
randomCompositionLength len n = go (n - 1) (len - 1, 1, [])
  where
    go np (mp, j, acc)
      | mp > 0 = go (np - 1) . bool (mp - 1, 1, j : acc) (mp, j + 1, acc) =<< randomIO
      | otherwise = pure (j + np : acc)
