import Data.Rhythm.Binary (necklacesAllowed)
import Options.Applicative

main :: IO ()
main =
  mapM_ (putStrLn . concatMap show)
    . uncurry (flip necklacesAllowed)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N" <> help "length")
            <*> some (argument auto (metavar "P1 P2 ..." <> help "allowed parts"))
        )
        (fullDesc <> progDesc "Binary necklaces with allowed parts.")
    p = prefs showHelpOnEmpty
