import Data.Finite (getFinite)
import Data.Rhythm.Binary (necklacesAllowed)
import Options.Applicative

main :: IO ()
main =
  mapM_ (putStrLn . concatMap (show . getFinite))
    . uncurry (flip necklacesAllowed)
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N")
            <*> some (argument auto (metavar "P1 P2 ..."))
        )
        (fullDesc <> progDesc "Binary necklaces with allowed parts.")
    p = prefs showHelpOnEmpty
