import Data.Rhythm.Binary (necklacesPopCount)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . map (concatMap show)
    . uncurry (flip necklacesPopCount)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N" <> help "length")
            <*> argument auto (metavar "M" <> help "number of ones")
        )
        (fullDesc <> progDesc "All binary necklaces with a given number of ones of a given length.")
    p = prefs showHelpOnEmpty
