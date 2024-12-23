module Data.Rhythm.ContinuedFractions where

import Data.Bifunctor (first)
import Data.Functor.Base (NonEmptyF (..))
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe)

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
