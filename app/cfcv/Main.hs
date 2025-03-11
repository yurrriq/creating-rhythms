import Control.Arrow ((&&&))
import qualified Data.List.NonEmpty as NE
import Data.Ratio (denominator, numerator)
import Data.Rhythm.ContinuedFractions (ContinuedFraction (..), collapseFraction)
import Options.Applicative
import Text.Printf (printf)

main :: IO ()
main = printFraction =<< customExecParser p opts
  where
    opts =
      info
        (some (argument auto (metavar "TERMS...")))
        (fullDesc <> progDesc "Calculate a continued fraction convergent.")
    p = prefs showHelpOnEmpty

printFraction :: [Integer] -> IO ()
printFraction =
  uncurry (printf "%d %d\n")
    . (numerator &&& denominator)
    . collapseFraction
    . ContinuedFraction
    . NE.fromList
