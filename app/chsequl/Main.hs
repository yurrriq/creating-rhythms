{-# LANGUAGE RecordWildCards #-}

import Data.Rhythm.Christoffel (christoffelWord)
import Options.Applicative

data Args = Args
  { isUpperWord :: !Bool,
    numerator :: !Integer,
    denominator :: !Integer,
    nTerms :: !(Maybe Int)
  }

main :: IO ()
main =
  do
    Args {..} <- customExecParser (prefs showHelpOnEmpty) args
    putStrLn $
      concatMap show $
        christoffelWord isUpperWord numerator denominator nTerms

args :: ParserInfo Args
args =
  info
    (Args <$> (upper <|> lower) <*> p <*> q <*> optional n)
    (fullDesc <> progDesc "Christoffel words for a given slope.")
  where
    upper =
      flag' True $
        long "upper" <> short 'u' <> help "upper Christoffel word"
    lower =
      flag' False $
        long "lower" <> short 'l' <> help "lower Christoffel word"
    p = argument auto (metavar "P" <> help "numerator")
    q = argument auto (metavar "Q" <> help "denominator")
    n = argument auto (metavar "N" <> help "number of terms (default P+Q)")
