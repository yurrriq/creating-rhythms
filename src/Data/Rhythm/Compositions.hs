module Data.Rhythm.Compositions where

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
compositionsLength :: (Integral a) => a -> a -> [Composition]
compositionsLength = compositions1

-- | Positive compositions of a given length with allowed parts.
--
-- >>> compositionsLengthAllowed 2 [1,2,3] 4
-- [[1,3],[2,2],[3,1]]
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
