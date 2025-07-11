import Data.Finite (getFinite)
import Data.Rhythm.Binary (necklacesPopCountAllowed)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . map (foldMap (show . getFinite))
    . (\(n, m, ps) -> necklacesPopCountAllowed m ps n)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,,)
            <$> argument auto (metavar "N")
            <*> argument auto (metavar "M")
            <*> some (argument auto (metavar "PART..."))
        )
        (fullDesc <> progDesc "All binary necklaces of length N with M ones and allowed parts.")
    p = prefs showHelpOnEmpty
