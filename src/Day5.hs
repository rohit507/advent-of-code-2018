module Day5 where


import Data.Char

import Debug.Trace

import Text.Show.Pretty

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Data.List

part1 :: IO ()
part1 = interact $ show . (\ x -> x - 1) .length . quiesce retract

part2 :: IO ()
part2 = interact $ show . (\x -> x - 1 ) . minimum . map (length . snd) . collapseEach

collapseEach :: String -> [(Char,String)]
collapseEach s = map (\ c -> traceShowId (c, quiesce retract $ collapse c s)) ['a' .. 'z']

quiesce :: (Eq a, Show a) => (a -> a) -> a -> a
quiesce f a = if a == f a then a else quiesce f (f a)

collapse :: Char -> String -> String
collapse _ [] = []
collapse c (x:xs)
  | toLower c == toLower x = collapse c xs
  | otherwise = x : (collapse c xs)

retract :: String -> String
retract [] = []
retract (x:[]) = [x]
retract (x:y:[]) | polarized x y = []
                 | otherwise     = [x,y]
retract (x:y:z:xs)
  | polarized x y = retract $ z:xs
  | polarized y z = retract $ x:xs
  | otherwise     = x : (retract $ y:z:xs)

polarized :: Char -> Char -> Bool
polarized a b = (a /= b) && (toUpper a == toUpper b )
