{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Rhythm.Binary
-- Copyright   : (c) Eric Bailey, 2024-2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
--
-- De Bruijn sequences, and conversion between binary strings and lists of
-- intervals.
module Data.Rhythm.Binary
  ( binaryToIntervals,
    deBruijnSequence,
    intervalsToBinary,
    RSW.necklaces,
    RSW.necklaces',
    necklacesAllowed,
    necklacesPopCount,
    necklacesPopCountAllowed,
  )
where

import Control.Lens (makeLenses, uses, (%=))
import Control.Monad (when)
import Control.Monad.State (State, evalState)
import Data.Bits (Bits (testBit), popCount)
import Data.FastDigits (undigits)
import Data.IntSet qualified as IntSet
import Data.List (unfoldr)
import Data.List.Extra (snoc, splitOn)
import Data.Rhythm.Binary.RuskeySavageWang qualified as RSW
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (flatten)

-- | Convert a binary string to a list of intervals.
--
-- >>> binaryToIntervals "1010010001001000"
-- [2,3,4,3,4]
binaryToIntervals :: String -> [Int]
binaryToIntervals =
  map ((+ 1) . length) . tail . splitOn "1"

data DeBruijnState = DeBruijnState
  { _seed :: [Int],
    _seen :: Set Integer
  }

makeLenses ''DeBruijnState

mkDeBruijnState :: Int -> DeBruijnState
mkDeBruijnState n = DeBruijnState (replicate n 0) Set.empty

-- | Generate the largest de Bruijn sequence of a given order.
--
-- Based on http://debruijnsequence.org/db/greedy.
--
-- >>> deBruijnSequence 4
-- [1,1,1,1,0,1,1,0,0,1,0,1,0,0,0,0]
deBruijnSequence :: Int -> [Int]
deBruijnSequence n = evalState (outer []) (mkDeBruijnState n)
  where
    outer neck =
      do
        seed %= (flip snoc 0 . tail)
        inner 1 neck >>= maybe (pure (reverse neck)) outer

    inner i neck =
      visit i >>= \case
        True -> pure (Just (i : neck))
        False | i > 0 -> inner (i - 1) neck
        False -> pure Nothing

visit :: Int -> State DeBruijnState Bool
visit v =
  do
    seed %= (flip snoc v . init)
    num <- uses seed (undigits @Int 2)
    isNew <- uses seen (Set.notMember num)
    when isNew $
      seen %= Set.insert num
    pure isNew

-- | Convert a list of intervals to a binary string.
--
-- >>> intervalsToBinary [2,3,4,3,4]
-- "1010010001001000"
intervalsToBinary :: [Int] -> String
intervalsToBinary =
  concatMap (('1' :) . (`replicate` '0') . subtract 1)

necklacesAllowed :: [Int] -> Int -> [[Int]]
necklacesAllowed allowed n =
  RSW.nodesToNecklaces n $
    filter ((> 0) <&&> isAllowed) $
      flatten (RSW.necklaces' n)
  where
    isAllowed = all (`IntSet.member` IntSet.fromList allowed) . countParts n

-- | All binary necklaces with a given number of ones of a given length.
--
-- >>> necklacesPopCount 3 6
-- [[1,1,1,0,0,0],[1,1,0,1,0,0],[1,0,1,1,0,0],[1,0,1,0,1,0]]
necklacesPopCount :: Int -> Int -> [[Int]]
necklacesPopCount !m !n =
  RSW.nodesToNecklaces n $
    filter ((== m) . popCount) $
      flatten (RSW.necklaces' n)

-- | All binary necklaces with a given number of ones of a given length with
-- allowed parts.
--
-- >>> necklacesPopCountAllowed 3 5 [1,2,3]
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

(<&&>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixr 3 <&&> -- same as (&&)
