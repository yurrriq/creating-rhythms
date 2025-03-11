import Data.List (sort)
import Data.Rhythm.Partitions (partitionsLengthAllowed)
import Math.Combinat.Partitions (toList)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . sort
    . map (unwords . map show . reverse . toList)
    . (\(n, m, ps) -> partitionsLengthAllowed m ps n)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,,)
            <$> argument auto (metavar "N")
            <*> argument auto (metavar "M")
            <*> some (argument auto (metavar "P1 P2 ..."))
        )
        (fullDesc <> progDesc "Partitions of a given length with allowed parts.")
    p = prefs showHelpOnEmpty
