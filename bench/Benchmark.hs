import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Functor ((<&>))
import Data.Rhythm.Binary.BurrowsWheeler qualified as BW
import Data.Rhythm.Binary.RuskeySavageWang qualified as RSW
import Text.Printf (printf)

main :: IO ()
main =
  defaultMain $
    [5, 10, 15] <&> \n ->
      bgroup
        (printf "Binary necklaces of length %d" n)
        [ bench "RSW" $ whnf RSW.necklaces n,
          bench "Burrows-Wheeler" $ whnf BW.necklaces n
        ]
