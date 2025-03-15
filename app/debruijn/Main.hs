import Data.Rhythm.Binary (deBruijnSequence)
import Options.Applicative

main :: IO ()
main =
  putStrLn
    . concatMap show
    . deBruijnSequence
    =<< customExecParser p opts
  where
    opts =
      info
        (argument auto (metavar "n"))
        (fullDesc <> progDesc "Generate all binary necklaces of length n.")
    p = prefs showHelpOnEmpty
