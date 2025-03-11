import Data.List (sort)
import Data.Rhythm.Partitions (partitionsAllowed)
import Math.Combinat.Partitions (toList)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . sort
    . map (unwords . map show . reverse . toList)
    . uncurry (flip partitionsAllowed)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N")
            <*> some (argument auto (metavar "P1 P2 ..."))
        )
        (fullDesc <> progDesc "Partitions with allowed parts.")
    p = prefs showHelpOnEmpty
