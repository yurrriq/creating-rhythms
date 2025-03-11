import Data.List (sort)
import Data.Rhythm.Partitions (partitionsLength)
import Math.Combinat.Partitions (toList)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . sort
    . map (unwords . map show . reverse . toList)
    . uncurry (flip partitionsLength)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N")
            <*> argument auto (metavar "M")
        )
        (fullDesc <> progDesc "Partitions of a given length.")
    p = prefs showHelpOnEmpty
