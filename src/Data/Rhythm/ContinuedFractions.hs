-- |
-- Module      : Data.Rhythm.ContinuedFractions
-- Copyright   : (c) Eric Bailey, 2024-2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- [Simple continued fractions](https://mathworld.wolfram.com/SimpleContinuedFraction.html)
-- represented by nonempty lists of terms.
--
-- \[
--   \begin{align*}
--     [b_0; b_1, b_2, b_3, \dotsc] &=
--     b_0 + \cfrac{1}{b_1 + \cfrac{1}{b_2 + \cfrac{1}{b_3 + \dotsm}}} \\
--     &= b_0 + \mathop{\vcenter{\Huge\mathcal{K}}}_{n=1}^{\infty} \frac{1}{b_n}
--   \end{align*}
-- \]
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
-- integer 'terms'.
--
-- >>> ContinuedFraction (1 :| [2,3,4])
-- [1;2,3,4]
newtype ContinuedFraction = ContinuedFraction
  {terms :: NonEmpty Integer}
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
--
-- >>> collapseFraction (ContinuedFraction (1 :| [2,3,4]))
-- 43 % 30
collapseFraction :: ContinuedFraction -> Rational
collapseFraction = cata (algebra . first toRational) . terms
  where
    algebra (NonEmptyF i Nothing) = i
    algebra (NonEmptyF i (Just d)) = i + recip d

-- | Calculate the 'ContinuedFraction' representation of the square root of a
-- given number.
--
-- >>> continuedFractionSqrt 7
-- [2;1,1,1,4]
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
          let b' = (n - a' * a') `div` b
          let c' = (root + a') `div` b'
          pure (a', b', c')
    root = truncate (sqrt @Double (fromIntegral n))
