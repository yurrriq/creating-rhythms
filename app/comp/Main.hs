{-# LANGUAGE TypeApplications #-}

import Data.List (sort)
import Data.Rhythm.Compositions (compositions)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . sort
    . map (unwords . map show)
    . compositions @Integer
    =<< customExecParser p opts
  where
    opts =
      info
        (argument auto (metavar "N"))
        (fullDesc <> progDesc "All positive compositions of a given number.")
    p = prefs showHelpOnEmpty
