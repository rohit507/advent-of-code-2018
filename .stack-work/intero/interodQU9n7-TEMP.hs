module Day2 where

import Control.Monad (guard)
import Data.List (sort,group)

part1 :: IO ()
part1 = interact (show . getChecksum . lines)

  where

    getChecksum :: Ord a => [[a]] -> Int
    getChecksum l = (length $ filter (hasN 2) l) * (length $ filter (hasN 3) l)

counts :: (Ord a) => [a] -> [(a,Int)]
counts = map (\ l -> (head l, length l)) . group . sort

hasN :: (Ord a) => Int -> [a] -> Bool
hasN n = elem n . map snd . counts

singleReplaced :: Eq a => [a] -> [a] -> Bool
singleReplaced [] _ = False
singleReplaced _ [] = False
singleReplaced (a:as) (b:bs)
  | (a == b)               = singleReplaced as bs
  | (a /= b) && (as == bs) = True
  | otherwise              = False


getCommon :: Eq a => [a] -> [a] -> [a]
getCommon [] _ = []
getCommon _ [] = []
getCommon (a:as) (b:bs)
  | a == b    = a : getCommon as bs
  | otherwise =     getCommon as bs

part2 :: IO ()
part2 = interact (show . uncurry getCommon . checkPairs . lines)

  where

    checkPairs :: Eq a => [[a]] -> ([a],[a])
    checkPairs l = head $ do
      w1 <- l
      w2 <- l
      guard $ singleReplaced w1 w2
      return (w1, w2)
