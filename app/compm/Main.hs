import Data.Rhythm.Compositions (compositionsLength)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . map (unwords . map show)
    . uncurry (flip compositionsLength)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N")
            <*> argument auto (metavar "M")
        )
        (fullDesc <> progDesc "Positive compositions of a given length.")
    p = prefs showHelpOnEmpty
