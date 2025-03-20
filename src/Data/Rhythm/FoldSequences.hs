-- |
-- Module      : Data.Rhythm.BDF
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
--
-- Fold sequences.
module Data.Rhythm.FoldSequences
  ( foldSequence,
  )
where

import Data.Bits (countTrailingZeros, shiftL, (.&.))
import Data.Bool (bool)

-- | Generate fold sequences from given number of terms, number of bits, and
-- function number \(\{0,\dotsc,2^m-1\}\).
--
-- >>> foldSequence 7 4 2
-- [0,1,1,0,0,0,1]
foldSequence :: Int -> Int -> Int -> [Int]
foldSequence n m f =
  [ bool x (1 - x) ((2 * b + 1) `mod` 4 > 1)
    | i <- [1 .. n],
      let j = i .&. (-i),
      let (a, b) = (countTrailingZeros j `mod` m, i `div` (2 * j)),
      let x = fromEnum (0 == (g .&. (1 `shiftL` a)))
  ]
  where
    g = pow2m - (f `mod` pow2m) - 1
    pow2m = 2 ^ m
