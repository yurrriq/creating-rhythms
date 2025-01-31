import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Functor ((<&>))
import Data.Rhythm.Binary (necklaces, necklaces')
import Text.Printf (printf)

main :: IO ()
main =
  defaultMain $
    [5, 10, 15, 20, 21, 22] <&> \n ->
      bgroup
        (printf "Binary necklaces of length %d" n)
        [ bench "RSW" $ whnf necklaces n,
          bench "Burrows-Wheeler" $ whnf necklaces' n
        ]
