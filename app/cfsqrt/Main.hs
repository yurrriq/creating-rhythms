{-# LANGUAGE TypeApplications #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Rhythm.ContinuedFractions (ContinuedFraction (..), continuedFractionSqrt)
import Options.Applicative
import Text.Printf (printf)

main :: IO ()
main =
  putStrLn
    . (\(n :| ds) -> printf "%d ( %s )" n (unwords (map show ds)))
    . terms
    . continuedFractionSqrt @Integer
    =<< customExecParser p opts
  where
    opts =
      info (argument auto (metavar "N")) $
        progDesc
          "Calculate the continued fraction representation of sqrt(N).\n\
          \The periodic part is in parentheses."
    p = prefs showHelpOnEmpty
