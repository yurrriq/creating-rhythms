{-# LANGUAGE LambdaCase #-}

import Data.Rhythm.Binary.BDF (SomeBinaryRhythmDefinition (..), someBinaryRhythmDefinition, toAbcString)
import Options.Applicative hiding (ParserResult (..))
import Text.Trifecta (ErrInfo (..), Result (..), parseFromFileEx)

main :: IO ()
main =
  do
    (filename, repeats) <- customExecParser p opts :: IO (String, Int)
    parseFromFileEx someBinaryRhythmDefinition filename >>= \case
      Failure (ErrInfo reason _) ->
        print reason
      Success (SomeBinaryRhythmDefinition rhythms) ->
        putStrLn $ toAbcString filename repeats rhythms
  where
    opts =
      info
        ( (,)
            <$> argument str (metavar "FILENAME" <> help "name of rhythm definition file")
            <*> argument auto (metavar "REPEATS" <> help "number of times to repeat the rhythm")
        )
        (fullDesc <> progDesc "Convert a binary rhythm set to ABC notation.")
    p = prefs showHelpOnEmpty
