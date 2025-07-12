{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Data.Rhythm.Binary.BDF
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Binary rhythm definitions, including conversion to ABC notation.
module Data.Rhythm.Binary.BDF
  ( BinaryRhythmDefinition (..),
    BinaryRhythm (..),
    SomeBinaryRhythmDefinition (..),
    someBinaryRhythmDefinition,
    toAbcString,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (imap)
import Data.Char (chr, ord)
import Data.Finite (Finite, getFinite)
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Text.Printf (printf)
import Text.Trifecta (Parser, char, count, natural, newline, skipOptional, (<?>))

-- | A binary rhythm definition consists of a tempo and \(m\) binary rhythms
-- consisting of \(n\) notes.
newtype BinaryRhythmDefinition (m :: Nat) (n :: Nat)
  = BinaryRhythmDefinition
      (Int, Vector m (BinaryRhythm n))
  deriving (Eq, Generic)

instance (KnownNat rhythmCount, KnownNat noteCount) => Show (BinaryRhythmDefinition rhythmCount noteCount) where
  show (BinaryRhythmDefinition (tempo, rhythms)) =
    intercalate "\n" $
      header : map show (VS.toList rhythms)
    where
      header =
        unwords
          [ show tempo,
            show (natVal (Proxy @noteCount)),
            show (natVal (Proxy @rhythmCount))
          ]

-- | Existential wrapper around a 'BinaryRhythmDefinition' of unknown size.
data SomeBinaryRhythmDefinition where
  SomeBinaryRhythmDefinition ::
    (KnownNat rhythmCount, KnownNat noteCount) =>
    BinaryRhythmDefinition rhythmCount noteCount ->
    SomeBinaryRhythmDefinition

instance Show SomeBinaryRhythmDefinition where
  show (SomeBinaryRhythmDefinition rhythms) = show rhythms

-- | A binary rhythm consists of a MIDI instrument number and \(n\) binary words
-- where a @1@ represents an onset.
newtype BinaryRhythm (n :: Nat) = BinaryRhythm
  {unBinaryRhythm :: (Int, Vector n (Finite 2))}
  deriving (Eq, Generic)

instance Show (BinaryRhythm n) where
  show (BinaryRhythm (instrumentNumber, notes)) =
    unwords [show instrumentNumber, concatMap (show . getFinite) notes]

-- | Parse a 'BinaryRhythmDefinition' of unknown size.
someBinaryRhythmDefinition :: Parser SomeBinaryRhythmDefinition
someBinaryRhythmDefinition =
  do
    tempo <- posInt <?> "tempo"
    noteCount <- posInt <?> "note count"
    rhythmCount <- posInt <?> "number of rhythms"
    case (someNatVal (fromIntegral noteCount), someNatVal (fromIntegral rhythmCount)) of
      (SomeNat (_ :: Proxy noteCount), SomeNat (_ :: Proxy rhythmCount)) ->
        VS.fromList @rhythmCount <$> count rhythmCount (binaryRhythm @noteCount) >>= \case
          Just rhythms ->
            pure $ SomeBinaryRhythmDefinition (BinaryRhythmDefinition (tempo, rhythms))
          Nothing -> fail "Ope!"

-- | Given a title and a number of repeats, represent a binary rhythm definition
-- using ABC notation.
toAbcString ::
  forall rhythmCount noteCount.
  (KnownNat rhythmCount, KnownNat noteCount) =>
  String ->
  Int ->
  BinaryRhythmDefinition rhythmCount noteCount ->
  String
toAbcString title repeats (BinaryRhythmDefinition (tempo, rhythms)) =
  intercalate "\n" $
    [ "X: 1",
      printf "T: %s" title,
      printf "M: %d/4" (natVal (Proxy @noteCount)),
      "K: C",
      printf "Q: %d" tempo
    ]
      ++ imap (showRhythm repeats) (VS.toList rhythms)

showRhythm :: Int -> Int -> BinaryRhythm n -> String
showRhythm repeats i (BinaryRhythm (instrumentNumber, notes)) =
  intercalate
    "\n"
    [ printf "V:%d clef=perc" (i + 1),
      "L: 1/4",
      "%%MIDI channel 10",
      printf "%%%%MIDI drummap %c %d" voiceChar instrumentNumber,
      tune
    ]
  where
    voiceChar = chr (ord 'A' + i)

    tune =
      printf "| %s |" . unwords . intersperse "|" . replicate repeats $
        concatMap showGroup (NE.group notes)

    showGroup grp@(1 :| _) = replicate (length grp) voiceChar
    showGroup grp = show (1 + length grp)

binaryRhythm :: forall n. (KnownNat n) => Parser (BinaryRhythm n)
binaryRhythm =
  do
    instrumentNumber <- posInt <?> "instrument number"
    maybeRhythm <- VS.fromList <$> count (fromIntegral (natVal (Proxy @n))) binaryDigit
    skipOptional newline
    case maybeRhythm of
      Just rhythm -> pure (BinaryRhythm @n (instrumentNumber, rhythm))
      Nothing -> fail "Invalid binary rhythm"

binaryDigit :: Parser (Finite 2)
binaryDigit = zero <|> one
  where
    zero = (0 <$ char '0') <?> "zero"
    one = (1 <$ char '1') <?> "one"

posInt :: Parser Int
posInt = fromInteger <$> natural
