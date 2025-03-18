{-# LANGUAGE TypeApplications #-}

import Data.List (sort)
import Data.Rhythm.Permutations (permutations)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . sort
    . map (unwords . map show)
    . permutations @Integer
    =<< customExecParser p opts
  where
    opts =
      info
        (some (argument auto (metavar "A1 A2 ...")))
        (fullDesc <> progDesc "Strictly lexicographically larger permutations of a given list of numbers.")
    p = prefs showHelpOnEmpty
