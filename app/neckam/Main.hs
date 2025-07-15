import Data.Rhythm.Binary (necklacesPopCountAllowed)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . map (concatMap show)
    . (\(n, m, ps) -> necklacesPopCountAllowed m ps n)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,,)
            <$> argument auto (metavar "N" <> help "length")
            <*> argument auto (metavar "M" <> help "number of ones")
            <*> some (argument auto (metavar "PART..." <> help "allowed parts"))
        )
        (fullDesc <> progDesc "All binary necklaces of length N with M ones and allowed parts.")
    p = prefs showHelpOnEmpty
