module Data.Rhythm.Partitions
  ( partitions,
    partitionsAllowed,
    partitionsLength,
    partitionsLengthAllowed,
  )
where

import Math.Combinat.Partitions
  ( Partition,
    fromPartition,
    partitions,
    partitionsWithKParts,
  )

-- | Partitions with allowed parts.
--
-- >>> partitionsAllowed [1,2,3] 4
-- [Partition [1,1,1,1],Partition [2,1,1],Partition [2,2],Partition [3,1]]
partitionsAllowed :: (Foldable t) => t Int -> Int -> [Partition]
partitionsAllowed allowed n =
  filter (all (`elem` allowed) . fromPartition) (partitions n)

-- | Partitions of a given length.
--
-- >>> partitionsLength 3 7
-- [Partition [3,2,2],Partition [3,3,1],Partition [4,2,1],Partition [5,1,1]]
partitionsLength :: Int -> Int -> [Partition]
partitionsLength = partitionsWithKParts

-- | foo bar baz
--
-- >>> partitionsLengthAllowed 2 [1,2,3] 4
-- [Partition [2,2],Partition [3,1]]
partitionsLengthAllowed :: (Foldable t) => Int -> t Int -> Int -> [Partition]
partitionsLengthAllowed len allowed =
  filter (all (`elem` allowed) . fromPartition) . partitionsLength len
