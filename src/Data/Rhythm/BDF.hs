{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Data.Rhythm.BDF
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
--
-- Binary rhythm definitions, including conversion to ABC notation.
module Data.Rhythm.BDF
  ( BinaryRhythmDefinition (..),
    BinaryRhythm (..),
    binaryRhythmDefinition,
    toAbcString,
  )
where

import Control.Applicative ((<|>))
import Data.Char (chr, ord)
import Data.Finite (Finite, getFinite)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Text.Printf (printf)
import Text.Trifecta (Parser, char, count, natural, newline, skipOptional, (<?>))

-- | A binary rhythm definition consists of a tempo, a note count \(n\), a
-- rhythm count \(m\), and a list of \(m\) binary rhythms consisting of \(n\)
-- notes.
data BinaryRhythmDefinition = BinaryRhythmDefinition
  { -- | beats per minute
    tempo :: !Int,
    -- | number of notes in each rhythm
    noteCount :: !Int,
    -- | number of rhythms
    rhythmCount :: !Int,
    -- | a list of binary rhythms
    rhythms :: ![BinaryRhythm]
  }
  deriving (Eq)

instance Show BinaryRhythmDefinition where
  show (BinaryRhythmDefinition {..}) =
    intercalate "\n" $
      unwords (map show [tempo, noteCount, rhythmCount]) : map show rhythms

-- | A binary rhythm consists of a MIDI instrument number and a list of binary
-- words where a @1@ represents an onset.
data BinaryRhythm = BinaryRhythm
  { -- | MIDI instrument number
    instrumentNumber :: !Int,
    -- | a list of zeros and ones
    notes :: ![Finite 2]
  }
  deriving (Eq)

instance Show BinaryRhythm where
  show (BinaryRhythm {..}) =
    unwords [show instrumentNumber, concatMap (show . getFinite) notes]

-- | Parse a binary rhythm definition.
binaryRhythmDefinition :: Parser BinaryRhythmDefinition
binaryRhythmDefinition =
  do
    theTempo <- posInt <?> "tempo"
    theNoteCount <- posInt <?> "note count"
    theRhythmCount <- posInt <?> "number of rhythms"
    theRhythms <- count theRhythmCount (binaryRhythm theNoteCount)
    pure $ BinaryRhythmDefinition theTempo theNoteCount theRhythmCount theRhythms

-- | Given a title and a number of repeats, represent a binary rhythm definition
-- using ABC notation.
toAbcString :: String -> Int -> BinaryRhythmDefinition -> String
toAbcString title repeats (BinaryRhythmDefinition {..}) =
  intercalate "\n" $
    "X: 1"
      : "T: " <> title
      : printf "M: %d/4" noteCount
      : "K: C"
      : printf "Q: %d" tempo
      : zipWith showRhythm [0 :: Int ..] rhythms
  where
    showRhythm i (BinaryRhythm {..}) =
      intercalate
        "\n"
        [ printf "V:%d clef=perc" (i + 1),
          "L: 1/4",
          "%%MIDI channel 10",
          printf "%%%%MIDI drummap %c %d" c instrumentNumber,
          tune
        ]
      where
        c = chr (ord 'A' + i)
        tune =
          (<> " |") $
            unwords $
              replicate repeats $
                printf "| %s" $
                  flip concatMap (NE.group notes) $ \grp@(digit :| _) ->
                    if 1 == digit
                      then replicate (length grp) c
                      else show (1 + length grp)

binaryRhythm :: Int -> Parser BinaryRhythm
binaryRhythm n =
  do
    theInstrumentNumber <- posInt <?> "instrument number"
    theRhythm <- count n binaryDigit
    skipOptional newline
    pure $ BinaryRhythm theInstrumentNumber theRhythm

binaryDigit :: Parser (Finite 2)
binaryDigit = zero <|> one
  where
    zero = (0 <$ char '0') <?> "zero"
    one = (1 <$ char '1') <?> "one"

posInt :: Parser Int
posInt = fromInteger <$> natural
