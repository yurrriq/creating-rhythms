import Data.Rhythm.Binary (necklaces)
import Options.Applicative

main :: IO ()
main = printNecklaces =<< customExecParser p opts
  where
    opts =
      info
        (argument auto (metavar "n" <> value 0))
        (fullDesc <> progDesc "Generate all binary necklaces of length n.")
    p = prefs showHelpOnEmpty

printNecklaces :: Int -> IO ()
printNecklaces = mapM_ (putStrLn . concatMap show) . necklaces
