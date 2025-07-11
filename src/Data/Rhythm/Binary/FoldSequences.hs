{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Rhythm.Binary.FoldSequences
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Binary fold sequences.
module Data.Rhythm.Binary.FoldSequences
  ( foldSequence,
  )
where

import Data.Bits (countTrailingZeros, shiftL, (.&.))
import Data.Bool (bool)
import Data.Finite (Finite, finite, getFinite)
import Data.Proxy (Proxy (..))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.TypeLits (KnownNat, natVal, type (^))

-- $setup
-- >>> import Data.Vector.Sized (toList)

-- | Generate fold sequences from given number of terms, number of bits, and
-- function number \(\{0,\dotsc,2^m-1\}\).
--
-- >>> map getFinite $ toList $ foldSequence @7 @2 3
-- [1,1,0,1,1,0,0]
foldSequence :: forall n m. (KnownNat n, KnownNat m) => Finite (2 ^ m) -> Vector n (Finite 2)
foldSequence f = VS.map go (VS.enumFromN @n @Int 1)
  where
    go i = bool x (1 - x) ((2 * b + 1) `mod` 4 > 1)
      where
        j = i .&. (-i)
        (a, b) = (countTrailingZeros j `mod` m, i `div` (2 * j))
        x = finite (fromIntegral (fromEnum (0 == (g .&. (1 `shiftL` a)))))
    m = fromIntegral (natVal (Proxy @m))
    g = pow2m - (getFinite f `mod` pow2m) - 1
    pow2m = 2 ^ m
