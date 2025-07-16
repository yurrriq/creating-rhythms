{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- |
-- Module      : Data.Rhythm.DeBruijn
-- Description : De Bruijn sequences
-- Copyright   : (c) Eric Bailey, 2024-2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Constructing de Bruijn sequences using [a greedy
-- algorithm](http://debruijnsequence.org/db/greedy).
--
-- = References
--   - De Bruijn Sequence and Universal Cycle Constructions
--     (2024). http://debruijnsequence.org.
module Data.Rhythm.DeBruijn
  ( -- * Ergonomic
    deBruijnSequence,

    -- * Safe
    deBruijnSequence',

    -- * Determining symbols
    DeBruijnState (..),
    initialDeBruijnState,
  )
where

import Control.Lens (makeLenses, uses, (%=))
import Control.Monad (when)
import Control.Monad.State (evalState)
import Data.FastDigits (undigits)
import Data.Finite (Finite, getFinite)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector, (//))
import Data.Vector.Sized qualified as VS
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal, type (^))

-- | Generator for a de Bruijn sequence of order @n@ and alphabet size @k@.
data DeBruijnState (k :: Nat) (n :: Nat) = DeBruijnState
  { _seed :: Vector n (Finite k),
    _seen :: Set Integer
  }

makeLenses ''DeBruijnState

-- | Seed with \(0^n\).
initialDeBruijnState :: (KnownNat k, KnownNat n) => DeBruijnState k n
initialDeBruijnState = DeBruijnState (VS.replicate 0) Set.empty

-- | Generate the largest de Bruijn sequence of a given order.
--
-- >>> concatMap show $ deBruijnSequence 2 4
-- "1111011001010000"
--
-- >>> concatMap show $ deBruijnSequence 4 3
-- "3332331330322321320312311310302301300222122021121020120011101000"
deBruijnSequence :: Integer -> Integer -> [Integer]
deBruijnSequence k n =
  case (someNatVal (fromInteger k), someNatVal (fromInteger n)) of
    (SomeNat (_ :: Proxy k), SomeNat (_ :: Proxy n)) ->
      getFinite <$> VS.toList (deBruijnSequence' @k @n)

-- | See 'deBruijnSequence'.
deBruijnSequence' :: forall k n. (KnownNat k, KnownNat n) => Vector (k ^ n) (Finite k)
deBruijnSequence' =
  evalState (VS.generateM (const (nextSymbol maxBound))) initialDeBruijnState
  where
    nextSymbol bit =
      try bit >>= \case
        True -> bit <$ (seed %= pushEnd @n bit)
        False -> nextSymbol (bit - 1)

    try bit =
      do
        seed %= (// [(maxBound, bit)])
        num <- uses seed (undigits k . VS.toList)
        isNew <- uses seen (Set.notMember num)
        when isNew $
          seen %= Set.insert num
        pure isNew

    k = natVal (Proxy @k)

-- | Push an item from the end of a vector.
--
-- Conceptually:
--
-- @
-- 'pushEnd' x v = 'VS.snoc' x ('VS.tail' v)
-- @
pushEnd :: (KnownNat n) => a -> VS.Vector n a -> VS.Vector n a
pushEnd x v = VS.generate $ \i ->
  if i == maxBound - 1
    then x
    else VS.index v (i + 1)
