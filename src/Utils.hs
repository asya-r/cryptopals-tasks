module Utils
  ( sortTupleListByFst
  ) where

import Data.List (sortBy)

sortTupleListByFst :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortTupleListByFst = sortBy (\(a, _) (b, _) -> compare a b)
