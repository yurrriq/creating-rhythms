module Data.Rhythm.Partitions
  ( partitions,
    partitionsLength,
  )
where

import Math.Combinat.Partitions (Partition, partitions, partitionsWithKParts)

-- | Partitions of a given length.
--
-- >>> partitionsLength 3 7
-- [Partition [3,2,2],Partition [3,3,1],Partition [4,2,1],Partition [5,1,1]]
partitionsLength :: Int -> Int -> [Partition]
partitionsLength = partitionsWithKParts
