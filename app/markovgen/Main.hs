import Data.Finite (finite, getFinite)
import Data.Foldable (toList)
import Data.Proxy (Proxy)
import Data.Rhythm.Markov (SomeTransitionMatrix (..), TransitionMatrix (..), markovGen, someTransitionMatrix)
import GHC.TypeNats (SomeNat (..), someNatVal)
import Options.Applicative
import Text.Trifecta (parseFromFile)

main :: IO ()
main =
  do
    (mfile, s, n) <- customExecParser (prefs showHelpOnEmpty) opts
    case someNatVal (fromIntegral n) of
      SomeNat (_ :: Proxy steps) ->
        do
          maybeMatrix <- parseFromFile someTransitionMatrix mfile
          case maybeMatrix of
            Just (SomeTransitionMatrix (matrix :: TransitionMatrix n)) ->
              print matrix
                *> markovGen @n @steps matrix (finite @n s)
                >>= putStrLn . unwords . map (show . getFinite) . toList
            Nothing -> fail "Invalid transition matrix"
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
