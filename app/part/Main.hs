import Data.List (sort)
import Data.Rhythm.Partitions (partitions)
import Math.Combinat.Partitions (toList)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . sort
    . map (unwords . map show . reverse . toList)
    . partitions
    =<< customExecParser p opts
  where
    opts =
      info
        (argument auto (metavar "N"))
        (fullDesc <> progDesc "Partitions of a given number.")
    p = prefs showHelpOnEmpty
