{-# LANGUAGE UnicodeSyntax #-}

module Data.Rhythm.Binary where

import Data.Bits (Bits (complementBit, rotateL))
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
    search necklace
      | necklace == m = [necklace]
      | otherwise = necklace : concatMap search candidates
      where
        candidates = takeWhile (isNecklace n m) (map τ (tail (σᵏ n m necklace)))
    m = 2 ^ n - 1

-- | Determine whether a given number represents a necklace of length @n@.
-- N.B. @m = 2 ^ n - 1@ for optimization purposes.
isNecklace :: (Integral a, Bits a) => Int -> a -> a -> Bool
isNecklace n m necklace =
  necklace == m
    || all (necklace <=) (σᵏ n m necklace)

-- | All rotations of a given binary necklace of length.
-- N.B. @m = 2 ^ n - 1@ for optimization purposes.
σᵏ :: (Integral a, Bits a) => Int -> a -> a -> [a]
σᵏ n m = take n . iterate (σ m)

-- | Rotate a given binary necklace left one bit.
-- N.B. @m = 2 ^ n - 1@ for optimization purposes.
σ :: (Integral a, Bits a) => a -> a -> a
σ m necklace = rotateL necklace 1 `mod` m

-- | Flip the least significant bit of a binary necklace.
τ :: (Bits a) => a -> a
τ = flip complementBit 0
