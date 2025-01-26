{-# LANGUAGE UnicodeSyntax #-}

module Data.Rhythm.Binary where

import Data.Bits (Bits (complementBit, rotateL, shiftL))
import Data.FastDigits (digits)
import Data.List (sortOn)
import Data.List.Extra (splitOn)
import Data.Ord (Down (..))
import Data.Tree (Tree (..), flatten, unfoldTree)

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
      flatten (necklacesRSW n)

-- | All binary necklaces of a given length, encoded as numbers.
--
-- \[
--   \begin{align*}
--     \sigma(x_1 ... x_n) &= x_2 ... x_n x_1 \\
--     \tau(x_1 ... x_{n-1}) &= x_1 ... x_{n-1}\overline{x_n}
--   \end{align*}
-- \]
--
-- Generate the tree of binary necklaces of length \(n\), starting with
-- \(x = 0^n\) as root, where children of \(x\) are the necklaces of the form
-- \(\tau\sigma^j(x)\) for \(1 \le j \le n -1\).
--
-- >>> flatten (necklacesRSW 4)
-- [0,1,3,7,15,5]
necklacesRSW :: (Integral a, Bits a) => Int -> Tree a
necklacesRSW 0 = Node 0 []
necklacesRSW n = Node 0 [unfoldTree search 1]
  where
    -- rotation
    σ necklace = rotateL necklace 1 `mod` m
    σʲ = take n . iterate σ

    τ = flip complementBit 0

    -- build the tree of find n-ary binary necklaces with a given root
    search necklace
      | necklace == m = (necklace, [])
      | otherwise = (necklace, takeWhile isNecklace (map τ (tail (σʲ necklace))))

    -- a necklace is the lexicographically smallest rotation
    isNecklace necklace =
      necklace == m
        || all (necklace <=) (σʲ necklace)

    -- 1ⁿ
    m = shiftL 1 n - 1
