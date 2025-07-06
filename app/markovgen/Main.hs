import Data.Rhythm.Markov (markovGen, transitionMatrix)
import Data.Vector qualified as V
import Options.Applicative
import Text.Trifecta (parseFromFile)

main :: IO ()
main =
  do
    (mfile, s, n) <- customExecParser (prefs showHelpOnEmpty) opts
    maybeMatrix <- parseFromFile transitionMatrix mfile
    numbers <- maybe (error "Oh no!") (markovGen n s) maybeMatrix
    putStrLn (unwords (map show (V.toList numbers)))
  where
    opts =
      info args $
        fullDesc <> progDesc "Generate random numbers using a Markov chain"

args :: Parser (FilePath, Int, Int)
args =
  (,,)
    <$> argument str (metavar "MFILE" <> help "transition matrix file name")
    <*> argument auto (metavar "S" <> help "starting state")
    <*> argument auto (metavar "N" <> help "how many random numbers to generate")
