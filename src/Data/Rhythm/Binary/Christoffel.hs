{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Module      : Data.Rhythm.Binary.Christoffel
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Christoffel words.
module Data.Rhythm.Binary.Christoffel
  ( christoffelWord,
    christoffelWord',
  )
where

import Control.Arrow (first, second)
import Data.Bool (bool)
import Data.Finite (Finite, getFinite, modulo)
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.TypeLits.Witnesses (SNat (..), (%+), (%-))
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal, type (+), type (-))

-- $setup
-- >>> import Data.Finite (getFinite)
-- >>> import Data.Ratio ((%))

-- | See 'christoffelWord''.
--
-- >>> christoffelWord False (3 % 7) 10
-- [0,0,0,1,0,0,1,0,0,1]
--
-- >>> christoffelWord True (3 % 7) 10
-- [1,0,0,1,0,0,1,0,0,0]
--
-- >>> christoffelWord True (3 % 7) 20
-- [1,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0]
christoffelWord :: Bool -> Rational -> Int -> [Integer]
christoffelWord isUpperWord slope nTerms =
  case (someNatVal (fromIntegral nTerms), someNatVal n, someNatVal d) of
    (SomeNat (_ :: Proxy k), SomeNat (_ :: Proxy n), SomeNat (_ :: Proxy d)) ->
      VS.toList $
        VS.map getFinite $
          christoffelWord' @k @n @d isUpperWord
  where
    (n, d) = (fromIntegral (numerator slope), fromIntegral (denominator slope))

-- | Generate the upper or lower Christoffel word for a given slope with a given
-- number of terms.
--
-- >>> map getFinite $ VS.toList $ christoffelWord' @10 @3 @7 False
-- [0,0,0,1,0,0,1,0,0,1]
--
-- >>> map getFinite $ VS.toList $ christoffelWord' @10 @3 @7 True
-- [1,0,0,1,0,0,1,0,0,0]
--
-- >>> map getFinite $ VS.toList $ christoffelWord' @20 @3 @7 True
-- [1,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0]
christoffelWord' ::
  forall m n d.
  (KnownNat m, KnownNat n, KnownNat d) =>
  Bool ->
  Vector m (Finite 2)
christoffelWord' isUpperWord =
  case (SNat :: SNat n) %+ ((SNat :: SNat d) %- (SNat :: SNat 1)) of
    SNat ->
      case (SNat :: SNat 1) %+ ((SNat :: SNat n) %+ ((SNat :: SNat d) %- (SNat :: SNat 1))) of
        SNat ->
          cycleVector $
            VS.cons (bool 0 1 isUpperWord) $
              VS.unfoldrN @(n + (d - 1)) go (n, d)
  where
    go slope = case uncurry compare slope of
      GT -> (1, second (+ d) slope)
      EQ -> (bool 1 0 isUpperWord, (n, d))
      LT -> (0, first (+ n) slope)
    n, d :: Integer
    n = fromIntegral (natVal (Proxy @n))
    d = fromIntegral (natVal (Proxy @d))

-- | Cycle a vector of length @n@ to produce a vector of length @m@.
cycleVector :: forall m n a. (KnownNat m, KnownNat n) => Vector n a -> Vector m a
cycleVector v = VS.unfoldrN go 0
  where
    go i = (v `VS.index` modulo i, i + 1)
