module Main where

main :: IO ()
main = day1Part2

day1Part1 :: IO ()
day1Part1 = interact (show . sum . map (read @Int . dropPlus) . lines)

  where

    dropPlus :: String -> String
    dropPlus ('+':xs) = xs
    dropPlus x = x

day1Part2 :: IO ()
day1Part2 = interact (show . scanl (+) 0 . map (read @Int . dropPlus) . lines)

  where

    dropPlus :: String -> String
    dropPlus ('+':xs) = xs
    dropPlus x = x
