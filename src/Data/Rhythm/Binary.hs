{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
    intervalsToBinary,
    RSW.necklaces,
    necklacesPopCount,
    deBruijnSequence,
  )
where

import Control.Lens (makeLenses, uses, (%=))
import Control.Monad (when)
import Control.Monad.State (State, evalState)
import Data.Bits (popCount)
import Data.FastDigits (undigits)
import Data.List.Extra (snoc, splitOn)
import qualified Data.Rhythm.Binary.RuskeySavageWang as RSW
import Data.Set (Set)
import qualified Data.Set as Set
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
