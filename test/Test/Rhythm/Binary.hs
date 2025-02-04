module Test.Rhythm.Binary where

import qualified Data.Rhythm.Binary.BurrowsWheeler as BW
import qualified Data.Rhythm.Binary.RuskeySavageWang as RSW
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Printf (printf)

test_necklaces :: TestTree
test_necklaces =
  testGroup "Binary necklaces" $
    [ testCase (printf "n = %d" n) $
        RSW.necklaces n @?= BW.necklaces n
      | n <- [5, 10, 15]
    ]
