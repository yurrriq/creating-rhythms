{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Rhythm.Binary.RuskeySavageWang
  ( necklaces,
    necklaces',
  )
where

import Data.Bits (Bits (complementBit, rotateL, shiftL))
import Data.FastDigits (digits)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Ord (Down (..))
import Data.Tree (Tree (..), flatten, unfoldTree)

-- | All binary necklaces of a given length.
--
-- >>> necklaces 4
-- [[1,1,1,1],[1,1,1,0],[1,1,0,0],[1,0,1,0],[1,0,0,0],[0,0,0,0]]
necklaces :: Int -> [[Int]]
necklaces !n =
  sortOn Down $
    map (padUpTo n . digits 2) $
      flatten (necklaces' n)

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
-- >>> flatten (necklaces' 4)
-- [0,1,3,7,15,5]
necklaces' :: (Integral a, Bits a) => Int -> Tree a
necklaces' 0 = Node 0 []
necklaces' !n = Node 0 [unfoldTree search 1]
  where
    -- rotation
    σ necklace = rotateL necklace 1 `mod` m
    σʲ = take (n - 1) . NE.tail . NE.iterate σ

    τ = flip complementBit 0

    -- build the tree of n-ary binary necklaces with a given root
    search !necklace
      | necklace == m = (necklace, [])
      | otherwise = (necklace, takeWhile isNecklace (map τ (σʲ necklace)))

    -- a necklace is the lexicographically smallest rotation
    isNecklace necklace =
      necklace == m
        || all (necklace <=) (σʲ necklace)

    -- 1ⁿ
    m = shiftL 1 n - 1

-- modified from Data.FastDigits
padUpTo :: Int -> [Int] -> [Int]
padUpTo !n [] = replicate n 0
padUpTo !n (x : xs) = x : padUpTo (n - 1) xs
