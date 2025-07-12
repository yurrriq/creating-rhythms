{-# LANGUAGE LambdaCase #-}

import Data.Rhythm.Markov (markovGen, someTransitionMatrix)
import Options.Applicative hiding (Failure, Success)
import Text.Trifecta (ErrInfo (..), Result (..), parseFromFileEx)

main :: IO ()
main =
  do
    (mfile, s, n) <- customExecParser (prefs showHelpOnEmpty) opts
    parseFromFileEx someTransitionMatrix mfile >>= \case
      Failure (ErrInfo reason _) ->
        print reason
      Success someMatrix ->
        print someMatrix
          >> markovGen someMatrix s n
          >>= putStrLn . unwords . map show
  where
    opts =
      info args $
        fullDesc <> progDesc "Generate random numbers using a Markov chain."

args :: Parser (FilePath, Integer, Integer)
args =
  (,,)
    <$> argument str (metavar "MFILE" <> help "transition matrix file name")
    <*> argument auto (metavar "S" <> help "starting state")
    <*> argument auto (metavar "N" <> help "how many random numbers to generate")
