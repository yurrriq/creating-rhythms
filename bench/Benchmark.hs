import Criterion.Main (bench, defaultMain, nf)
import Data.Functor ((<&>))
import Data.Rhythm.Binary.Necklaces (necklaces')
import Text.Printf (printf)

main :: IO ()
main =
  defaultMain $
    [5, 10, 15, 20] <&> \n ->
      bench (printf "binary necklaces of length %d" n) $
        nf (necklaces' @Integer) n
