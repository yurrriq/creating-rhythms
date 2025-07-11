{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module      : Data.Rhythm.Binary.Necklaces.RuskeySavageWang
-- Copyright   : (c) Eric Bailey, 2025
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
module Data.Rhythm.Binary.Necklaces.RuskeySavageWang
  ( necklaces,
    necklaces',
    nodesToNecklaces,
  )
where

import Data.Bits (Bits (complementBit, rotateL, shiftL))
import Data.FastDigits (digits)
import Data.Finite (Finite, finite)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Ord (Down (..))
import Data.Tree (Tree (..), flatten, unfoldTree)

-- $setup
-- >>> import Data.Finite (getFinite)

-- | All binary necklaces of a given length.
--
-- >>> map getFinite <$> necklaces 4
-- [[1,1,1,1],[1,1,1,0],[1,1,0,0],[1,0,1,0],[1,0,0,0],[0,0,0,0]]
necklaces :: Int -> [[Finite 2]]
necklaces !n =
  nodesToNecklaces n $
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

-- | Convert a list of nodes to binary necklaces of a given length.
--
-- >>> map getFinite <$> nodesToNecklaces 4 [3,5]
-- [[1,1,0,0],[1,0,1,0]]
nodesToNecklaces :: Int -> [Integer] -> [[Finite 2]]
nodesToNecklaces !n =
  sortOn Down
    . map (padUpTo n . map (finite . toInteger) . digits 2)

-- modified from Data.FastDigits
padUpTo :: (Num a) => Int -> [a] -> [a]
padUpTo !n [] = replicate n 0
padUpTo !n (x : xs) = x : padUpTo (n - 1) xs
