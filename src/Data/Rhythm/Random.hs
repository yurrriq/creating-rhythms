{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Rhythm.Random where

import Control.Monad (foldM, (>=>))
import Control.Monad.State.Strict (MonadIO, evalStateT, gets, liftIO)
import Control.Monad.Trans.State.Strict (modifyM)
import Data.Bool (bool)
import Data.Finite (Finite, finite, getFinite)
import Data.Proxy (Proxy (..))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.TypeLits (KnownNat, natVal, type (+))
import System.Random (randomIO)

-- | Generate a vector of random numbers with specified correlation.
randomInt ::
  forall m x y.
  (MonadIO m, KnownNat (x + 1), KnownNat y) =>
  Finite (x + 1) ->
  Finite (x + 1) ->
  m (Vector (1 + y) (Finite (x + 1)))
randomInt startingNumber correlation =
  VS.cons startingNumber
    <$> evalStateT (VS.replicateM generateNumbers) (getFinite startingNumber)
  where
    generateNumbers = gets finite <* modifyM applyCorrelation

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

    c = getFinite correlation
    m = natVal (Proxy @(x + 1)) - 1
