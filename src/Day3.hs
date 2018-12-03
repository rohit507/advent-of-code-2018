module Day3 where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

part1 :: IO ()
part1 = interact $ show . Map.size . Map.filter (> 1) . collectCellCounts . map readClaim . lines

part2 :: IO ()
part2 = interact $ show . runPart2 . map readClaim . lines

runPart2 :: [Claim] -> [Claim]
runPart2 claims = filter (not . doesClaimOverlap) claims

  where

    filteredMap = Map.filter (> 1) $ collectCellCounts claims

    doesClaimOverlap :: Claim -> Bool
    doesClaimOverlap c = any (flip Map.member filteredMap) (getCells c)


data Claim = Claim
  { id :: Int
  , left :: Int
  , top :: Int
  , width :: Int
  , height :: Int
  } deriving (Show)

readClaim :: String -> Claim
readClaim s =
  case parse claimParser "STDIN" s of
    Left err -> error (show err)
    Right claim -> claim

type LocCount = Map Loc Int
type Loc = (Int,Int)

-- | Gets a list of all cells that are in the claim
getCells :: Claim -> [Loc]
getCells Claim{..} = do
  xs <- map (left +) [0 .. width  - 1]
  ys <- map (top  +) [0 .. height - 1]
  return (xs,ys)

collectCellCounts :: [Claim] -> LocCount
collectCellCounts claims = foldl incrementCellCount Map.empty cells
  where

    cells = concatMap getCells claims

    incrementCellCount :: LocCount -> Loc -> LocCount
    incrementCellCount map loc = Map.insertWith (+) loc 1 map

claimParser :: Parser Claim
claimParser = do
  oneOf "#"
  id <- read <$> many1 digit
  space
  oneOf "@"
  space
  left <- read  <$> many1 digit
  oneOf ","
  top  <- read <$> many1 digit
  oneOf ":"
  space
  width <- read <$> many1 digit
  oneOf "x"
  height <- read <$> many1 digit
  return $ Claim {..}


--  #1 @ 258,327: 19x22
