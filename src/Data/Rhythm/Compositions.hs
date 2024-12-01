module Data.Rhythm.Compositions where

import Data.Bool (bool)
import System.Random (randomIO)

-- | Generate a random composition of a given 'Int'.
--
-- >>> sum <$> randomComposition 13
-- 13
randomComposition :: Int -> IO [Int]
randomComposition n = go 1 (1, [])
  where
    go i (p, acc)
      | i == n = pure (p : acc)
      | otherwise = go (i + 1) . bool (1, p : acc) (p + 1, acc) =<< randomIO
