import Data.Rhythm.Binary (necklacesPopCount)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . map (foldMap show)
    . uncurry (flip necklacesPopCount)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N")
            <*> argument auto (metavar "M")
        )
        (fullDesc <> progDesc "All binary necklaces with a given number of ones of a given length.")
    p = prefs showHelpOnEmpty
