{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Module      : Data.Rhythm.Random
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Generate random numbers with specified correlation.
module Data.Rhythm.Random where

import Control.Monad (foldM, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (unfoldrM)
import Data.Bool (bool)
import Data.Finite (Finite, finite, getFinite)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.TypeLits (KnownNat, natVal, type (+))
import System.Random (randomIO)

-- | Generate a vector of random numbers with specified correlation.
randomFinites ::
  forall x y m.
  (KnownNat x, KnownNat y, MonadIO m) =>
  Finite x ->
  Finite x ->
  m (Vector (1 + y) (Finite x))
randomFinites startingNumber correlation =
  VS.cons startingNumber . VS.map finite . fromJust . VS.fromListN
    <$> unfoldrM go (natVal (Proxy @y), getFinite startingNumber)
  where
    go (0, _) = pure Nothing
    go (n, prev) =
      applyCorrelation prev <&> \next ->
        Just (next, (n - 1, next))

    applyCorrelation =
      flip (foldM doDecrement) [m, m - 1 .. m - c + 1]
        >=> flip (foldM doIncrement) [1 .. c]

    doDecrement prev divisor =
      bool prev (max 0 (prev - 1))
        . (< fromIntegral prev / fromIntegral divisor)
        <$> liftIO (randomIO @Double)

    doIncrement prev _ =
      bool prev (min m (prev + 1))
        <$> liftIO randomIO

    m = natVal (Proxy @x) - 1
    c = getFinite correlation
