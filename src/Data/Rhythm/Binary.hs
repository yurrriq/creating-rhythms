module Data.Rhythm.Binary where

import Data.List.Extra (splitOn)

-- | Convert a binary string to a list of intervals.
--
-- >>> binaryToIntervals "1010010001001000"
-- [2,3,4,3,4]
binaryToIntervals :: String -> [Int]
binaryToIntervals =
  map (succ . length) . tail . splitOn "1"

-- | Convert a list of intervals to a binary string.
--
-- >>> intervalsToBinary [2,3,4,3,4]
-- "1010010001001000"
intervalsToBinary :: [Int] -> String
intervalsToBinary =
  concatMap (('1' :) . (`replicate` '0') . pred)
