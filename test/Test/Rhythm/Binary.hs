module Test.Rhythm.Binary where

import Data.Rhythm.Binary.Necklaces (necklaces)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Printf (printf)

test_necklaces :: TestTree
test_necklaces =
  testGroup "Binary necklaces" $
    [ testCase (printf "n = %d" n) $
        length (necklaces n) @?= k
      | (n, k) <- [(5, 8), (10, 108), (15, 2192)]
    ]
