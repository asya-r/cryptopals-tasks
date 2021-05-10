module Ch8
  ( ch8
  , countDupBlocks
  ) where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import Data.List (nub, sortBy)

import Utils (from16)

ch8 :: BS.ByteString
ch8 = findAES_ECB $ encode <$> (unsafePerformIO $ lines <$> readFile "files/8.txt") where
  encode = from16 . BS8.pack

findAES_ECB :: [BS.ByteString] -> BS.ByteString
findAES_ECB bss = last $ sortBy (\x y -> compare (count x) (count y)) bss where
  count = countDuplicates . blocks

countDupBlocks :: BS.ByteString -> Int
countDupBlocks = countDuplicates . blocks

blocks :: BS.ByteString -> [BS.ByteString]
blocks bs = hlp bs [] where
  hlp bs bls = if BS.length bs < 16
               then bls
               else hlp (BS.drop 16 bs) (bls ++ [BS.take 16 bs])

countDuplicates :: (Ord a) => [a] -> Int
countDuplicates xs = (length xs) - (length (nub xs))
