import Data.Rhythm.BDF (binaryRhythmDefinition, toAbcString)
import Options.Applicative
import Text.Trifecta (parseFromFile)

main :: IO ()
main =
  do
    (filename, repeats) <- customExecParser p opts :: IO (String, Int)
    parseFromFile binaryRhythmDefinition filename
      >>= maybe (error "No parse") (putStrLn . toAbcString filename repeats)
  where
    opts =
      info
        ( (,)
            <$> argument str (metavar "FILENAME" <> help "name of rhythm definition file")
            <*> argument auto (metavar "REPEATS" <> help "number of times to repeat the rhythm")
        )
        (fullDesc <> progDesc "Convert a binary rhythm set to abc notation.")
    p = prefs showHelpOnEmpty
