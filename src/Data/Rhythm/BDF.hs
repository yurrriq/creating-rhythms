{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Rhythm.BDF
  ( BinaryRhythm (..),
    BinaryRhythmDefinition (..),
    binaryRhythmDefinition,
    toAbcString,
  )
where

import Control.Applicative ((<|>))
import Data.Char (chr, ord)
import Data.Finite (Finite, getFinite)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Text.Printf (printf)
import Text.Trifecta (Parser, char, count, natural, newline, skipOptional, some, (<?>))

data BinaryRhythm = BinaryRhythm
  { instrumentNumber :: !Int,
    rhythm :: ![Finite 2]
  }
  deriving (Eq)

instance Show BinaryRhythm where
  show (BinaryRhythm {..}) =
    unwords [show instrumentNumber, concatMap (show . getFinite) rhythm]

data BinaryRhythmDefinition = BinaryRhythmDefinition
  { tempo :: !Int,
    noteCount :: !Int,
    rhythmCount :: !Int,
    rhythms :: ![BinaryRhythm]
  }
  deriving (Eq)

instance Show BinaryRhythmDefinition where
  show (BinaryRhythmDefinition {..}) =
    intercalate "\n" $
      unwords (map show [tempo, noteCount, rhythmCount]) : map show rhythms

binaryRhythmDefinition :: Parser BinaryRhythmDefinition
binaryRhythmDefinition =
  do
    theTempo <- posInt
    theNoteCount <- posInt
    theRhythmCount <- posInt
    theRhythms <- count theRhythmCount (ritmo <* skipOptional newline)
    pure $ BinaryRhythmDefinition theTempo theNoteCount theRhythmCount theRhythms
  where
    ritmo = BinaryRhythm <$> posInt <*> some binaryDigit

binaryDigit :: Parser (Finite 2)
binaryDigit =
  ((0 <$ char '0') <?> "zero")
    <|> ((1 <$ char '1') <?> "one")

posInt :: Parser Int
posInt = fromInteger <$> natural

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
                  flip concatMap (NE.group rhythm) $ \grp@(digit :| _) ->
                    if 1 == digit
                      then replicate (length grp) c
                      else show (1 + length grp)
