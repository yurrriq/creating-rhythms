import Data.Rhythm.Compositions (randomComposition)
import Options.Applicative

main :: IO ()
main =
  putStrLn
    . unwords
    . map show
    =<< randomComposition
    =<< customExecParser p opts
  where
    opts =
      info
        (argument auto (metavar "N"))
        (fullDesc <> progDesc "Generate a random positive composition of a given number.")
    p = prefs showHelpOnEmpty
