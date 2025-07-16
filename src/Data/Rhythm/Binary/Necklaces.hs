{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module      : Data.Rhythm.Binary.Necklaces
-- Description : Binary necklaces
-- Copyright   : (c) Eric Bailey, 2024-2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Binary [necklaces](http://combos.org/necklace), internally encoded as
-- numbers.
--
-- = References
--   - Frank Ruskey, Carla Savage, Terry Min Yih Wang, Generating necklaces,
--     Journal of Algorithms, Volume 13, Issue 3, 1992, Pages 414-430, ISSN
--     0196-6774, https://doi.org/10.1016/0196-6774(92)90047-G.
module Data.Rhythm.Binary.Necklaces
  ( -- * Ergonomic
    necklaces,
    necklacesAllowed,
    necklacesPopCount,
    necklacesPopCountAllowed,

    -- * Efficient
    necklaces',
  )
where

import Control.Bool ((<&&>))
import Data.Bits (Bits (complementBit, rotateL, shiftL), popCount)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NE
import Data.Rhythm.Internal (countParts, nodesToNecklaces)
import Data.Tree (Tree (..), flatten, unfoldTree)

-- $setup
-- >>> import Data.Finite (getFinite)

-- | All binary necklaces of a given length.
--
-- >>> necklaces 4
-- [[1,1,1,1],[1,1,1,0],[1,1,0,0],[1,0,1,0],[1,0,0,0],[0,0,0,0]]
necklaces :: Int -> [[Int]]
necklaces !n =
  nodesToNecklaces n $
    flatten (necklaces' n)

-- | All binary necklaces of a given length with allowed parts.
--
-- >>> map getFinite <$> necklacesAllowed [1,2,3] 5
-- [[1,1,1,1,1],[1,1,1,1,0],[1,1,1,0,0],[1,1,0,1,0],[1,0,1,0,0]]
necklacesAllowed :: [Int] -> Int -> [[Int]]
necklacesAllowed allowed n =
  nodesToNecklaces n $
    filter ((> 0) <&&> isAllowed) $
      flatten (necklaces' n)
  where
    isAllowed = all (`IntSet.member` IntSet.fromList allowed) . countParts n

-- | All binary necklaces with a given number of ones of a given length.
--
-- >>> map getFinite <$> necklacesPopCount 3 6
-- [[1,1,1,0,0,0],[1,1,0,1,0,0],[1,0,1,1,0,0],[1,0,1,0,1,0]]
necklacesPopCount :: Int -> Int -> [[Int]]
necklacesPopCount !m !n =
  nodesToNecklaces n $
    filter ((== m) . popCount) $
      flatten (necklaces' n)

-- | All binary necklaces with a given number of ones of a given length with
-- allowed parts.
--
-- >>> map getFinite <$> necklacesPopCountAllowed 3 [1,2,3] 5
-- [[1,1,1,0,0],[1,1,0,1,0]]
necklacesPopCountAllowed :: Int -> [Int] -> Int -> [[Int]]
necklacesPopCountAllowed m allowed n =
  nodesToNecklaces n $
    filter ((> 0) <&&> ((== m) . popCount) <&&> isAllowed) $
      flatten (necklaces' n)
  where
    isAllowed = all (`IntSet.member` IntSet.fromList allowed) . countParts n

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
