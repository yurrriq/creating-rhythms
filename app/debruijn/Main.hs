import Data.Rhythm.DeBruijn (deBruijnSequence)
import Options.Applicative

main :: IO ()
main =
  putStrLn
    . concatMap show
    . uncurry (flip deBruijnSequence)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N" <> help "order")
            <*> argument auto (metavar "K" <> help "alphabet size" <> showDefault <> value 2)
        )
        (fullDesc <> progDesc "Generate the largest de Bruijn sequence.")
    p = prefs showHelpOnEmpty
