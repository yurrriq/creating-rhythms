-- |
-- Module      : Data.Rhythm.Permutations
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
--
-- Strictly lexicographically larger permutations of a given list of numbers.
module Data.Rhythm.Permutations where

import Data.List qualified as List

-- | Strictly lexicographically larger permutations of a given list of numbers.
--
-- >>> permutations [1,2,3]
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
-- >>> permutations [3,1,2]
-- [[3,1,2],[3,2,1]]
permutations :: (Integral a) => [a] -> [[a]]
permutations xs = xs : filter (> xs) (List.permutations xs)
