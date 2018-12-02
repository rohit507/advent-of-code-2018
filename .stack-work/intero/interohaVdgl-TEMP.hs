module Day1 where

import Data.List (inits, tails)
import Data.Set (Set)
import qualified Data.Set as Set

part1 :: IO ()
part1 = interact (show . sum . map (read @Int . dropPlus) . lines)

dropPlus :: String -> String
dropPlus ('+':xs) = xs
dropPlus x = x

readNums :: String -> [Int]
readNums = map (read @Int . dropPlus) . lines

part2 :: IO ()
part2 = interact (getDups . readNums)

getDups :: [Int] -> String
getDups l = show . fst . head $ filter (uncurry Set.member) (zip sums prefixes)

 where

   looped = cycle l

   sums = scanl (+) 0 looped

   prefixes = scanl (flip Set.insert) Set.empty sums
