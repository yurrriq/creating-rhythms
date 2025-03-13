{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Rhythm.Binary
-- Copyright   : (c) Eric Bailey, 2024-2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
--
-- Conversion between binary strings and lists of intervals.
module Data.Rhythm.Binary
  ( binaryToIntervals,
    intervalsToBinary,
    RSW.necklaces,
    necklacesPopCount,
  )
where

import Data.Bits (popCount)
import Data.List.Extra (splitOn)
import qualified Data.Rhythm.Binary.RuskeySavageWang as RSW
import Data.Tree (flatten)

-- | Convert a binary string to a list of intervals.
--
-- >>> binaryToIntervals "1010010001001000"
-- [2,3,4,3,4]
binaryToIntervals :: String -> [Int]
binaryToIntervals =
  map ((+ 1) . length) . tail . splitOn "1"

-- | Convert a list of intervals to a binary string.
--
-- >>> intervalsToBinary [2,3,4,3,4]
-- "1010010001001000"
intervalsToBinary :: [Int] -> String
intervalsToBinary =
  concatMap (('1' :) . (`replicate` '0') . subtract 1)

-- | All binary necklaces with a given number of ones of a given length.
--
-- >>> necklacesPopCount 3 6
-- [[1,1,1,0,0,0],[1,1,0,1,0,0],[1,0,1,1,0,0],[1,0,1,0,1,0]]
necklacesPopCount :: Int -> Int -> [[Int]]
necklacesPopCount !m !n =
  RSW.nodesToNecklaces n $
    filter ((== m) . popCount) $
      flatten (RSW.necklaces' n)
