{-# LANGUAGE UnicodeSyntax #-}

module Data.Rhythm.Binary where

import Data.Bits (Bits (complementBit, rotateL, shiftL))
import Data.FastDigits (digits)
import Data.List (sortOn)
import Data.List.Extra (splitOn)
import Data.Ord (Down (..))

-- | Convert a binary string to a list of intervals.
--
-- >>> binaryToIntervals "1010010001001000"
-- [2,3,4,3,4]
binaryToIntervals :: String -> [Int]
binaryToIntervals =
  map (succ . length) . tail . splitOn "1"

-- | Convert a list of intervals to a binary string.
--
-- >>> intervalsToBinary [2,3,4,3,4]
-- "1010010001001000"
intervalsToBinary :: [Int] -> String
intervalsToBinary =
  concatMap (('1' :) . (`replicate` '0') . pred)

-- | All binary necklaces of a given length.
--
-- >>> necklaces 4
-- [[1,1,1,1],[1,1,1,0],[1,1,0,0],[1,0,1,0],[1,0,0,0],[0,0,0,0]]
necklaces :: Int -> [[Int]]
necklaces n =
  sortOn Down $
    map (take n . (++ repeat 0) . digits 2) $
      necklacesRSW n

-- | All binary necklaces of a given length, encoded as numbers.
--
-- >>> necklacesRSW 4
-- [0,1,3,7,15,5]
necklacesRSW :: (Integral a, Bits a) => Int -> [a]
necklacesRSW 0 = [0]
necklacesRSW n = 0 : search 1
  where
    -- rotation
    σ necklace = rotateL necklace 1 `mod` m
    σᵏ = take n . iterate σ

    -- negate least significant bit
    τ = flip complementBit 0

    -- Find n-ary binary necklaces
    search necklace
      | necklace == m = [necklace]
      | otherwise = necklace : concatMap search candidates
      where
        candidates = takeWhile isNecklace (map τ (tail (σᵏ necklace)))

    -- a necklace is the lexicographically smallest rotation
    isNecklace necklace =
      necklace == m
        || all (necklace <=) (σᵏ necklace)

    -- 1ⁿ
    m = shiftL 1 n - 1
