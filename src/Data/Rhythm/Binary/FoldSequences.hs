{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- |
-- Module      : Data.Rhythm.Binary.FoldSequences
-- Description : Binary fold sequences
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Binary fold sequences.
module Data.Rhythm.Binary.FoldSequences
  ( -- * Ergonomic
    foldSequence,

    -- * Safe
    foldSequence',
  )
where

import Data.Bits (countTrailingZeros, shiftL, (.&.))
import Data.Bool (bool)
import Data.Finite (Finite, finite, getFinite)
import Data.Proxy (Proxy (..))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal, type (^))

-- | See 'foldSequence''.
--
-- >>> foldSequence 7 2 3
-- [1,1,0,1,1,0,0]
foldSequence :: Int -> Integer -> Integer -> [Integer]
foldSequence n m f =
  case (someNatVal (fromIntegral n), someNatVal (fromIntegral m)) of
    (SomeNat (_ :: Proxy n), SomeNat (_ :: Proxy m)) ->
      foldr ((:) . getFinite) [] $
        foldSequence' @n @m (finite f)

-- | Generate a fold sequence of @n@ terms with @m@ bits from function number
-- @f@ \(\in \{0,\dotsc,2^m-1\}\).
--
-- >>> map getFinite $ VS.toList $ foldSequence' @7 @2 3
-- [1,1,0,1,1,0,0]
foldSequence' :: forall n m. (KnownNat n, KnownNat m) => Finite (2 ^ m) -> Vector n (Finite 2)
foldSequence' f = VS.map go (VS.enumFromN @n @Int 1)
  where
    go i = bool x (1 - x) ((2 * b + 1) `mod` 4 > 1)
      where
        j = i .&. (-i)
        (a, b) = (countTrailingZeros j `mod` m, i `div` (2 * j))
        x = finite (fromIntegral (fromEnum (0 == (g .&. (1 `shiftL` a)))))
    m = fromIntegral (natVal (Proxy @m))
    g = pow2m - (getFinite f `mod` pow2m) - 1
    pow2m = 2 ^ m
