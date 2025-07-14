{-# LANGUAGE RecordWildCards #-}

import Control.Arrow ((&&&))
import Data.Ratio (denominator, numerator, (%))
import Data.Rhythm.Binary.Christoffel (christoffelWord)
import Options.Applicative

data Args = Args
  { isUpperWord :: Bool,
    slope :: Rational,
    nTerms :: Int
  }

main :: IO ()
main =
  do
    Args {..} <- customExecParser (prefs showHelpOnEmpty) args
    putStrLn $
      concatMap show $
        christoffelWord isUpperWord slope nTerms

args :: ParserInfo Args
args =
  info
    (Args <$> (upper <|> lower) <*> slope <*> n)
    (fullDesc <> progDesc "Christoffel words for a given slope.")
  where
    upper =
      flag' True $
        long "upper" <> short 'u' <> help "upper Christoffel word"
    lower =
      flag' False $
        long "lower" <> short 'l' <> help "lower Christoffel word"
    slope = (%) <$> p <*> q
    p = argument auto (metavar "P" <> help "numerator")
    q = argument auto (metavar "Q" <> help "denominator")
    n =
      argument auto (metavar "N" <> help "number of terms (default P+Q)")
        <|> (fromIntegral . uncurry (+) . (numerator &&& denominator) <$> slope)
