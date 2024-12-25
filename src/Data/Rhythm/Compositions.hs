module Data.Rhythm.Compositions where

import Data.Bool (bool)
import Math.Combinat.Compositions (Composition)
import System.Random (randomIO)

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
