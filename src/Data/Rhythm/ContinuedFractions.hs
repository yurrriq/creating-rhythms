{-# LANGUAGE TypeApplications #-}

module Data.Rhythm.ContinuedFractions
  ( ContinuedFraction (..),
    collapseFraction,
    continuedFractionSqrt,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Functor.Base (NonEmptyF (..))
import Data.Functor.Foldable (ana, cata)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe)
import Data.Tuple.Extra (thd3)

-- | A 'ContinuedFraction' is a 'NonEmpty', potentially infinite, list of
-- integer 'coefficients'.
newtype ContinuedFraction = ContinuedFraction
  {coefficients :: NonEmpty Integer}
  deriving (Eq, Ord)

instance Show ContinuedFraction where
  show (ContinuedFraction (x :| xs)) =
    "["
      <> show x
      <> ";"
      <> intercalate "," (fmap show ys)
      <> maybe "" (const ",...") (listToMaybe zs)
      <> "]"
    where
      (ys, zs) = splitAt 11 xs

-- | Evaluate a finite 'ContinuedFraction'.
collapseFraction :: ContinuedFraction -> Rational
collapseFraction = cata (algebra . first toRational) . coefficients
  where
    algebra (NonEmptyF i Nothing) = i
    algebra (NonEmptyF i (Just d)) = i + recip d

-- | Calculate the 'ContinuedFraction' representation of the square root of a
-- given number.
continuedFractionSqrt :: (Integral a) => a -> ContinuedFraction
continuedFractionSqrt n
  | root * root < n =
      ContinuedFraction $ fromIntegral . thd3 <$> ana coalgebra (0, 1, root)
  | otherwise =
      ContinuedFraction (fromIntegral root :| [])
  where
    coalgebra (a, b, c) =
      NonEmptyF (a, b, c) $
        do
          guard (c /= 2 * root)
          let a' = b * c - a
          let b' = (n' - a' * a') `div` b
          let c' = (root + a') `div` b'
          pure (a', b', c')
    root = isqrt n
    n' = fromIntegral n

isqrt :: (Integral a) => a -> a
isqrt = truncate . sqrt @Double . fromIntegral
