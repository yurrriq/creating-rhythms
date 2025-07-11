{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Module      : Data.Rhythm.Markov
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Generating random numbers using a Markov chain.
module Data.Rhythm.Markov
  ( TransitionMatrix (..),
    SomeTransitionMatrix (..),
    markovGen,
    someTransitionMatrix,
  )
where

import Control.Monad.State.Strict (MonadIO, evalStateT, liftIO)
import Control.Monad.Trans.State.Strict (get, modifyM)
import Data.Finite (Finite)
import Data.List (intercalate)
import Data.Proxy (Proxy)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), someNatVal)
import System.Random (randomIO)
import Text.Printf (printf)
import Text.Trifecta (Parser, count, decimal, double, newline)

-- | An \(n \times n\) transition matrix.
newtype TransitionMatrix (n :: Nat) = TransitionMatrix
  { unTransitionMatrix :: Vector n (Vector n Double)
  }
  deriving (Eq, Generic)

instance Show (TransitionMatrix n) where
  show (TransitionMatrix matrix) =
    intercalate "\n" $
      map (unwords . map (printf "%.6f") . VS.toList) $
        VS.toList matrix

-- | Existential wrapper around a square 'TransitionMatrix' of unknown size.
data SomeTransitionMatrix where
  SomeTransitionMatrix :: (KnownNat n) => TransitionMatrix n -> SomeTransitionMatrix

deriving instance Show SomeTransitionMatrix

-- | Parse a square 'TransitionMatrix' of unknown size.
someTransitionMatrix :: Parser SomeTransitionMatrix
someTransitionMatrix =
  do
    n <- fromInteger <$> decimal <* newline
    rows <- count n (count n double)
    case someNatVal (fromIntegral n) of
      SomeNat (_ :: Proxy n) ->
        case traverse (VS.fromList @n) rows >>= VS.fromList of
          Just vec -> pure (SomeTransitionMatrix (TransitionMatrix vec))
          Nothing -> fail "Invalid transition matrix"

-- | Generate random numbers using a Markov chain.
markovGen ::
  forall n steps m.
  (KnownNat n, KnownNat steps, MonadIO m, MonadFail m) =>
  TransitionMatrix n ->
  Finite n ->
  m (Vector steps (Finite n))
markovGen (TransitionMatrix matrix) = evalStateT (VS.generateM go)
  where
    go = const (get <* modifyM (step . VS.index matrix))

    step row =
      liftIO randomIO >>= \p ->
        maybe (fail "Invalid transition matrix row") pure $
          VS.findIndex (> p) (VS.tail (VS.scanl (+) 0 row))
