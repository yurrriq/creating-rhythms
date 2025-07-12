{-# LANGUAGE RecordWildCards #-}

import Data.Rhythm.Random (randomNumbers)
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
    putStrLn . unwords . map show
      =<< randomNumbers maxNumber startingNumber degreeOfCorrelation numbersToGenerate

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
