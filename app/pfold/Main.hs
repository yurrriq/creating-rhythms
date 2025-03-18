import Data.Rhythm.FoldSequences (foldSequence)
import Options.Applicative

main :: IO ()
main =
  customExecParser p opts >>= \(n, m, f) ->
    putStrLn (concatMap show (foldSequence n m f))
  where
    opts = info args (fullDesc <> progDesc "Generate fold sequences.")
    p = prefs showHelpOnEmpty

args :: Parser (Int, Int, Int)
args =
  (,,)
    <$> argument auto (metavar "N" <> help "the number of terms")
    <*> argument auto (metavar "M" <> help "number of bits")
    <*> argument auto (metavar "F" <> help "function number 0 -> 2^m-1")
