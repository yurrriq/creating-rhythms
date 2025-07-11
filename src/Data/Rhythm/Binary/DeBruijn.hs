{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Rhythm.Binary.DeBruijn
-- Copyright   : (c) Eric Bailey, 2024-2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- De Bruijn sequences, and conversion between binary strings and lists of
-- intervals.
module Data.Rhythm.Binary.DeBruijn
  ( deBruijnSequence,
  )
where

import Control.Lens (makeLenses, uses, (%=))
import Control.Monad (when)
import Control.Monad.State (State, evalState)
import Data.FastDigits (undigits)
import Data.Finite (Finite)
import Data.List.Extra (snoc)
import Data.Set (Set)
import Data.Set qualified as Set

data DeBruijnState = DeBruijnState
  { _seed :: [Finite 2],
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
deBruijnSequence :: Int -> [Finite 2]
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

visit :: Finite 2 -> State DeBruijnState Bool
visit v =
  do
    seed %= (flip snoc v . init)
    num <- uses seed (undigits @Int 2)
    isNew <- uses seen (Set.notMember num)
    when isNew $
      seen %= Set.insert num
    pure isNew
