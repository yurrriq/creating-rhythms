import Data.Finite (getFinite)
import Data.Rhythm.Binary (deBruijnSequence)
import Options.Applicative

main :: IO ()
main =
  putStrLn
    . concatMap (show . getFinite)
    . deBruijnSequence
    =<< customExecParser p opts
  where
    opts =
      info
        (argument auto (metavar "n"))
        (fullDesc <> progDesc "Generate the largest de Bruijn sequence of a given order.")
    p = prefs showHelpOnEmpty
