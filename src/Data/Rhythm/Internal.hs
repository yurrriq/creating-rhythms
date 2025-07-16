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
  ( -- * Parsers
    binaryDigit,
  )
where

import Control.Applicative ((<|>))
import Data.Finite (Finite)
import Text.Trifecta (Parser, char, (<?>))

-- $setup
-- >>> import Text.Trifecta (parseString)

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
