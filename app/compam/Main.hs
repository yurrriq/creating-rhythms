import Data.Rhythm.Compositions (compositionsLengthAllowed)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . map (unwords . map show)
    . (\(n, m, ps) -> compositionsLengthAllowed m ps n)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,,)
            <$> argument auto (metavar "N")
            <*> argument auto (metavar "M")
            <*> some (argument auto (metavar "P1 P2 ..."))
        )
        (fullDesc <> progDesc "Positive compositions of a given length with allowed parts.")
    p = prefs showHelpOnEmpty
