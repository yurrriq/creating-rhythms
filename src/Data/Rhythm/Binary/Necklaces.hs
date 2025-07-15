{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Data.Rhythm.Binary.Necklaces
-- Copyright   : (c) Eric Bailey, 2024-2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Binary necklaces.
module Data.Rhythm.Binary.Necklaces
  ( RSW.necklaces,
    RSW.necklaces',
    RSW.nodesToNecklaces,
    necklacesAllowed,
    necklacesPopCount,
    necklacesPopCountAllowed,
  )
where

import Control.Bool ((<&&>))
import Data.Bits (Bits (testBit), popCount)
import Data.IntSet qualified as IntSet
import Data.List (unfoldr)
import Data.Rhythm.Binary.Necklaces.RuskeySavageWang qualified as RSW
import Data.Tree (flatten)

-- $setup
-- >>> import Data.Finite (getFinite)

-- | All binary necklaces of a given length with allowed parts.
--
-- >>> map getFinite <$> necklacesAllowed [1,2,3] 5
-- [[1,1,1,1,1],[1,1,1,1,0],[1,1,1,0,0],[1,1,0,1,0],[1,0,1,0,0]]
necklacesAllowed :: [Int] -> Int -> [[Int]]
necklacesAllowed allowed n =
  RSW.nodesToNecklaces n $
    filter ((> 0) <&&> isAllowed) $
      flatten (RSW.necklaces' n)
  where
    isAllowed = all (`IntSet.member` IntSet.fromList allowed) . countParts n

-- | All binary necklaces with a given number of ones of a given length.
--
-- >>> map getFinite <$> necklacesPopCount 3 6
-- [[1,1,1,0,0,0],[1,1,0,1,0,0],[1,0,1,1,0,0],[1,0,1,0,1,0]]
necklacesPopCount :: Int -> Int -> [[Int]]
necklacesPopCount !m !n =
  RSW.nodesToNecklaces n $
    filter ((== m) . popCount) $
      flatten (RSW.necklaces' n)

-- | All binary necklaces with a given number of ones of a given length with
-- allowed parts.
--
-- >>> map getFinite <$> necklacesPopCountAllowed 3 [1,2,3] 5
-- [[1,1,1,0,0],[1,1,0,1,0]]
necklacesPopCountAllowed :: Int -> [Int] -> Int -> [[Int]]
necklacesPopCountAllowed m allowed n =
  RSW.nodesToNecklaces n $
    filter ((> 0) <&&> ((== m) . popCount) <&&> isAllowed) $
      flatten (RSW.necklaces' n)
  where
    isAllowed = all (`IntSet.member` IntSet.fromList allowed) . countParts n

-- | Count the parts in the n-digit little-endian binary representation of x.
--
-- A part is the length of a substring 10* composing the necklace.
-- For example the necklace 10100 has parts of size 2 and 3.
--
-- >>> countParts 5 5
-- [2,3]
countParts :: (Integral a, Bits a) => Int -> a -> [Int]
countParts n x = unfoldr go (0, 0)
  where
    go (i, 0)
      | i >= n = Nothing
      | testBit x i = go (i + 1, 1)
      | otherwise = go (i + 1, 0)
    go (i, len)
      | i >= n = Just (len, (i, 0))
      | testBit x i = Just (len, (i + 1, 1))
      | otherwise = go (i + 1, len + 1)
