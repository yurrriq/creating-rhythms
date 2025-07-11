{-# LANGUAGE RecordWildCards #-}

import Data.Finite (finite, getFinite)
import Data.Foldable (toList)
import Data.Proxy (Proxy)
import Data.Rhythm.Random (randomFinites)
import GHC.TypeNats (SomeNat (..), someNatVal)
import Options.Applicative

data Args = Args
  { maxNumber :: Integer,
    startingNumber :: Integer,
    degreeOfCorrelation :: Integer,
    numbersToGenerate :: Integer
  }

main :: IO ()
main =
  do
    Args {..} <- customExecParser (prefs showHelpOnEmpty) args
    case (someNatVal (fromIntegral maxNumber + 1), someNatVal (fromIntegral numbersToGenerate - 1)) of
      (SomeNat (_ :: Proxy x), SomeNat (_ :: Proxy y)) ->
        let s = finite @x startingNumber
            c = finite @x degreeOfCorrelation
         in randomFinites @x @y s c
              >>= putStrLn . unwords . map (show . getFinite) . toList

args :: ParserInfo Args
args =
  info
    (Args <$> m <*> s <*> c <*> n)
    (fullDesc <> progDesc "Generate random numbers with specified correlation.")
  where
    m = integerArg "M" "maximum number"
    s = integerArg "S" "starting number, 0 through M"
    c = integerArg "C" "degree of correlation, 0 = total correlation, M = no correlation"
    n = integerArg "N" "how many random numbers to generate"
    integerArg var desc = argument auto (metavar var <> help desc)
