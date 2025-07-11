import Data.Finite (getFinite)
import Data.Maybe (fromMaybe)
import Data.Rhythm.Binary (necklaces)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Text.Read (readMaybe)

main :: IO ()
main = printNecklaces =<< customExecParser p opts
  where
    opts =
      info
        (argument int (metavar "n"))
        (fullDesc <> progDesc "Generate all binary necklaces of length n.")
    int = fromMaybe 0 . readMaybe <$> readerAsk
    p = prefs showHelpOnEmpty

printNecklaces :: Int -> IO ()
printNecklaces = mapM_ (putStrLn . concatMap (show . getFinite)) . necklaces
