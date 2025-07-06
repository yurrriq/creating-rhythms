-- |
-- Module      : Data.Rhythm.Markov
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
--
-- Generating random numbers using a Markov chain.
module Data.Rhythm.Markov
  ( TransitionMatrix (..),
    transitionMatrix,
    markovGen,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Data.Vector (Vector)
import Data.Vector qualified as V
import System.Random (randomIO)
import Text.Trifecta (Parser, count, decimal, double, newline)

data TransitionMatrix = TransitionMatrix
  { size :: Int,
    unTransitionMatrix :: Vector (Vector Double)
  }
  deriving (Eq, Show)

transitionMatrix :: Parser (Vector (Vector Double))
transitionMatrix =
  do
    n <- fromInteger <$> decimal <* newline
    V.fromList <$> count n (V.fromList <$> count n double)

-- | Given how many numbers to create, the initial state, and a transition
-- matrix, generate random numbers using a Markov chain.
markovGen :: (MonadIO m) => Int -> Int -> Vector (Vector Double) -> m (Vector Int)
markovGen 0 _ _ = pure V.empty
markovGen n s m = V.cons s <$> V.unfoldrM go (1, m V.! s)
  where
    go (k, row)
      | k < n =
          randomIO <&> \p ->
            V.findIndex (> p) (V.scanl1 (+) row) <&> \i ->
              (i, (k + 1, m V.! i))
      | otherwise = pure Nothing
