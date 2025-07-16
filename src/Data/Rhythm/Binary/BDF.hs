{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Data.Rhythm.Binary.BDF
-- Description : Binary rhythm definitions
-- Copyright   : (c) Eric Bailey, 2025
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
--
-- Binary rhythm definitions, including conversion to ABC notation.
module Data.Rhythm.Binary.BDF
  ( -- * Types
    BinaryRhythmDefinition (..),
    BinaryRhythm (..),
    SomeBinaryRhythmDefinition (..),
    MIDIInstrument,

    -- * Parsing
    someBinaryRhythmDefinition,
    binaryRhythm,

    -- * Rendering
    toAbcString,
  )
where

import Closed (Closed (..))
import Control.Lens (imap)
import Data.Char (chr, ord)
import Data.Finite (Finite, getFinite)
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.Rhythm.Internal (binaryDigit)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as VS
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Text.Printf (printf)
import Text.Trifecta (Parser, count, natural, newline, skipOptional, (<?>))

-- | A binary rhythm definition consists of a tempo and @m@ binary rhythms
-- consisting of @n@ notes each.
newtype BinaryRhythmDefinition (m :: Nat) (n :: Nat)
  = BinaryRhythmDefinition
      (Int, Vector m (BinaryRhythm n))
  deriving (Eq, Generic)

instance (KnownNat m, KnownNat n) => Show (BinaryRhythmDefinition m n) where
  show (BinaryRhythmDefinition (tempo, rhythms)) =
    intercalate "\n" $
      header : map show (VS.toList rhythms)
    where
      header =
        unwords
          [ show tempo,
            show (natVal (Proxy @n)),
            show (natVal (Proxy @m))
          ]

-- | Existential wrapper around a 'BinaryRhythmDefinition' of unknown size.
data SomeBinaryRhythmDefinition where
  SomeBinaryRhythmDefinition ::
    (KnownNat m, KnownNat n) =>
    BinaryRhythmDefinition m n ->
    SomeBinaryRhythmDefinition

instance Show SomeBinaryRhythmDefinition where
  show (SomeBinaryRhythmDefinition rhythms) = show rhythms

-- | A binary rhythm consists of a MIDI instrument number and a binary words of
-- length @n@, where a @1@ represents an onset.
newtype BinaryRhythm (n :: Nat) = BinaryRhythm
  {unBinaryRhythm :: (MIDIInstrument, Vector n (Finite 2))}
  deriving (Eq, Generic)

instance Show (BinaryRhythm n) where
  show (BinaryRhythm (instrumentNumber, notes)) =
    unwords [show instrumentNumber, concatMap (show . getFinite) notes]

-- | The General MIDI Level 1 sound set consists of instruments numbered @1@
-- through @128@.
type MIDIInstrument = Closed 1 128

-- | Parse a 'BinaryRhythmDefinition' of unknown size.
someBinaryRhythmDefinition :: Parser SomeBinaryRhythmDefinition
someBinaryRhythmDefinition =
  do
    tempo <- fromInteger <$> natural <?> "tempo"
    n <- natural <?> "note count"
    m <- fromInteger <$> natural <?> "number of rhythms"
    case (someNatVal (fromIntegral n), someNatVal (fromIntegral m)) of
      (SomeNat (_ :: Proxy n), SomeNat (_ :: Proxy m)) ->
        do
          rhythms <- count m (binaryRhythm @n)
          maybe
            (fail "Invalid binary rhythm definition")
            (pure . SomeBinaryRhythmDefinition . BinaryRhythmDefinition . (tempo,))
            (VS.fromList @m rhythms)

-- | Parse a 'BinaryRhythm' of known length.
binaryRhythm :: forall n. (KnownNat n) => Parser (BinaryRhythm n)
binaryRhythm =
  do
    instrumentNumber <- fromIntegral <$> natural <?> "instrument number"
    notes <- count (fromIntegral (natVal (Proxy @n))) binaryDigit
    skipOptional newline
    case VS.fromList notes of
      Just rhythm -> return (BinaryRhythm (instrumentNumber, rhythm))
      Nothing -> fail "Invalid binary rhythm definition"

-- | Given a title and a number of repeats, represent a binary rhythm definition
-- using ABC notation.
toAbcString ::
  forall m n.
  (KnownNat m, KnownNat n) =>
  String ->
  Int ->
  BinaryRhythmDefinition m n ->
  String
toAbcString title repeats (BinaryRhythmDefinition (tempo, rhythms)) =
  intercalate "\n" $
    [ "X: 1",
      printf "T: %s" title,
      printf "M: %d/4" (natVal (Proxy @n)),
      "K: C",
      printf "Q: %d" tempo
    ]
      ++ imap (showRhythm repeats) (VS.toList rhythms)

-- | Given a number of repeats and a rhythm set index, represent a binary rhythm
-- using ABC notation.
showRhythm :: Int -> Int -> BinaryRhythm n -> String
showRhythm repeats i (BinaryRhythm (instrumentNumber, notes)) =
  intercalate
    "\n"
    [ printf "V:%d clef=perc" (i + 1),
      "L: 1/4",
      "%%MIDI channel 10",
      printf "%%%%MIDI drummap %c %d" voiceChar (toInteger instrumentNumber),
      tune
    ]
  where
    voiceChar = chr (ord 'A' + i)

    tune =
      printf "| %s |" . unwords . intersperse "|" . replicate repeats $
        concatMap showGroup (NE.group notes)

    showGroup grp@(1 :| _) = replicate (length grp) voiceChar
    showGroup grp = show (1 + length grp)
