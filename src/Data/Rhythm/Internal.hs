{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Module      : Data.Rhythm.Internal
-- Description : Here be dragons.
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module Data.Rhythm.Internal
  ( -- * Digits
    countParts,
    padUpTo,

    -- * Parsers
    binaryDigit,

    -- * Necklaces
    nodesToNecklaces,

    -- * Vectors
    cycleVector,
  )
where

import Control.Applicative ((<|>))
import Data.Bits (Bits, testBit)
import Data.FastDigits (digits)
import Data.Finite (Finite, modulo)
import Data.List (sortOn, unfoldr)
import Data.Ord (Down (..))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.TypeNats (KnownNat)
import Text.Trifecta (Parser, char, (<?>))

-- $setup
-- >>> import Text.Trifecta (parseString)

-- | Count the parts in the n-digit little-endian binary representation of x.
--
-- A part is the length of a substring \(10^*\) composing the necklace.
-- For example the necklace \(10100\) has parts sizes \(2\) and \(3\).
--
-- >>> countParts 5 5
-- [2,3]
countParts :: (Integral a, Bits a) => Int -> a -> [Int]
countParts n x = unfoldr go (0, 0)
  where
    go (i, 0)
      | i >= n = Nothing
      | testBit x i = go (i + 1, 1)
      | otherwise = go (i + 1, 0)
    go (i, len)
      | i >= n = Just (len, (i, 0))
      | testBit x i = Just (len, (i + 1, 1))
      | otherwise = go (i + 1, len + 1)

-- | Right pad a list with zeros up to a given length.
--
-- >>> padUpTo 5 [0,1]
-- [0,1,0,0,0]
padUpTo :: (Num a) => Int -> [a] -> [a]
padUpTo !n [] = replicate n 0
padUpTo !n (x : xs) = x : padUpTo (n - 1) xs

-- | Parse a binary digit, i.e., @0@ or @1@.
--
-- >>> parseString binaryDigit mempty "0"
-- Success (finite 0)
--
-- >>> parseString binaryDigit mempty "1"
-- Success (finite 1)
--
-- >>> parseString binaryDigit mempty "?"
-- Failure (ErrInfo {_errDoc = (interactive):1:1: error: expected: one, zero
-- 1 | ?<EOF>
--   | ^      , _errDeltas = [Columns 0 0]})
binaryDigit :: Parser (Finite 2)
binaryDigit = zero <|> one
  where
    zero = (0 <$ char '0') <?> "zero"
    one = (1 <$ char '1') <?> "one"

-- | Convert a list of nodes to binary necklaces of a given length.
--
-- >>> nodesToNecklaces 4 [3,5]
-- [[1,1,0,0],[1,0,1,0]]
nodesToNecklaces :: Int -> [Integer] -> [[Int]]
nodesToNecklaces !n =
  sortOn Down
    . map (padUpTo n . digits 2)

-- | Cycle a vector of length @n@ to produce a vector of length @m@.
--
-- Conceptually @'take' m '.' 'cycle'@, but for 'Vector's.
cycleVector :: forall m n a. (KnownNat m, KnownNat n) => Vector n a -> Vector m a
cycleVector v = VS.unfoldrN go 0
  where
    go i = (v `VS.index` modulo i, i + 1)
