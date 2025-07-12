import Data.Rhythm.Binary.FoldSequences (foldSequence)
import Data.Tuple.Extra (uncurry3)
import Options.Applicative

main :: IO ()
main = do
  customExecParser p opts
    >>= putStrLn . foldMap show . uncurry3 foldSequence
  where
    opts = info args (fullDesc <> progDesc "Generate fold sequences.")
    p = prefs showHelpOnEmpty

args :: Parser (Int, Integer, Integer)
args =
  (,,)
    <$> argument auto (metavar "N" <> help "the number of terms")
    <*> argument auto (metavar "M" <> help "number of bits")
    <*> argument auto (metavar "F" <> help "function number 0 -> 2^m-1")
