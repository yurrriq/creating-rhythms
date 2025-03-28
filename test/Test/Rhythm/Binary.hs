module Test.Rhythm.Binary where

import Data.Rhythm.Binary.BurrowsWheeler qualified as BW
import Data.Rhythm.Binary.RuskeySavageWang qualified as RSW
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
