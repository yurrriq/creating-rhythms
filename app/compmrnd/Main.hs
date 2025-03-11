import Data.Rhythm.Compositions (randomCompositionLength)
import Options.Applicative

main :: IO ()
main =
  putStrLn
    . unwords
    . map show
    =<< uncurry (flip randomCompositionLength)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N")
            <*> argument auto (metavar "M")
        )
        (fullDesc <> progDesc "Generate a random positive composition of a given length.")
    p = prefs showHelpOnEmpty
