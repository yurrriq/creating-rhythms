{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Arrow (second)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (unfoldrM)
import Data.Finite (Finite)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Slist (len)
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

instance Show SomeTransitionMatrix where
  show (SomeTransitionMatrix matrix) = show matrix

instance IsList SomeTransitionMatrix where
  type Item SomeTransitionMatrix = [Double]

  fromList rows =
    fromMaybe (error "Invalid transition matrix") $
      case someNatVal (fromIntegral (len (fromList rows))) of
        SomeNat (_ :: Proxy n) ->
          SomeTransitionMatrix . TransitionMatrix
            <$> (VS.fromListN @n =<< traverse VS.fromListN rows)

  toList (SomeTransitionMatrix (TransitionMatrix matrix)) =
    VS.toList <$> VS.toList matrix

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
markovGen (TransitionMatrix matrix) start =
  fromJust . (VS.fromListN @steps)
    <$> unfoldrM (go . second (VS.index matrix)) (steps, start)
  where
    go (0, _) = pure Nothing
    go (n, prev) =
      liftIO randomIO <&> \p ->
        VS.findIndex (> p) (VS.tail (VS.scanl (+) 0 prev)) <&> \next ->
          (next, (n - 1, next))
    steps = natVal (Proxy @steps)
