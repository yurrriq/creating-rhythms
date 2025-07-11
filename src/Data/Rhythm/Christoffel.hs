-- |
-- Module      : Data.Rhythm.Christoffel
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Christoffel words.
module Data.Rhythm.Christoffel
  ( christoffelWord,
  )
where

import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)

-- | Generate the upper or lower Christoffel word for a given slope with a given
-- number of terms.
--
-- >>> christoffelWord False 3 7 Nothing
-- [0,0,0,1,0,0,1,0,0,1]
--
-- >>> christoffelWord True 3 7 Nothing
-- [1,0,0,1,0,0,1,0,0,0]
--
-- >>> christoffelWord True 3 7 (Just 20)
-- [1,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0]
christoffelWord :: Bool -> Integer -> Integer -> Maybe Int -> [Integer]
christoffelWord isUpperWord numerator denominator nTerms =
  NE.take k $
    NE.cycle $
      bool 0 1 isUpperWord :| go 1 numerator denominator
  where
    go !i !a !b
      | i >= n = []
      | otherwise =
          case compare a b of
            GT -> 1 : go (i + 1) a (b + denominator)
            EQ -> bool 1 0 isUpperWord : go (i + 1) numerator denominator
            LT -> 0 : go (i + 1) (a + numerator) b
    n = fromInteger (numerator + denominator)
    k = fromMaybe n nTerms
