module Day4 where


import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List

part1 :: IO ()
part1 = interact $ show . go . map parseLogEntry . lines
  where
    go :: [LogEntry] -> Int
    go l = let dat = getSleepTimes l
               grd = getSleepiestGuard dat
               min = getSleepiestMin $ dat Map.! grd
           in grd * min

part2 :: IO ()
part2 = interact $ show . go . map parseLogEntry . lines
  where
    go l = let dat = getSleepTimes l
               guardSleeps = fmap getSleepiestMin' dat
               maxGrdMin (g,(m,s)) (g',(m',s')) = if s' > s then (g',(m',s')) else (g,(m,s))
               (grd,(min,_)) = foldl maxGrdMin (-1,(-1,-1)) (Map.assocs guardSleeps)
           in grd * min

data Action
  = Guard Int
  | Sleep
  | Wake
  deriving (Show, Eq,  Ord)

data TimeStamp = Time
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  } deriving (Show, Eq, Ord)

type Date = (Int,Int,Int)
type Time = (Int,Int)

type Guard = Int
type IsAsleep = Bool
type FellAsleep = Int
type Min = Int
type SleepTimes = Map Date [Min]
type GuardData = Map Guard SleepTimes
type SleepHist = Map Min Int

type SleepState = (Guard, IsAsleep, FellAsleep, GuardData)

getSleepiestGuard :: GuardData -> Guard
getSleepiestGuard = fst . foldl maxSleep (-1,-1) . Map.assocs . fmap getMinutesSlept
  where
    maxSleep (n,s) (n', s') = if s' > s then (n',s') else (n,s)

getSleepiestMin :: SleepTimes -> Min
getSleepiestMin = fst . getSleepiestMin'

getSleepiestMin' :: SleepTimes -> (Min,Int)
getSleepiestMin' = foldl maxMinute (-1,-1) . counts . concat . Map.elems
  where
    maxMinute (n,s) (n', s') = if s' > s then (n',s') else (n,s)

getSleepTimes :: [LogEntry] -> GuardData
getSleepTimes logs =
  let (_, _, _, d) = foldl updateLogs startState (sort logs)
   in d
  where
    startState = (-1, False, -1, Map.empty)
    updateLogs :: SleepState -> LogEntry -> SleepState
    -- updateLogs (guard, True, timeFall, dat) (time, Guard n) =
    --   ( n
    --   , False
    --   , -1
    --   , Map.insertWith
    --       (Map.union)
    --       guard
    --       (Map.singleton (getDate time) [timeFall .. 59])
    --       dat)
    updateLogs (_, False, -1, dat) (time, Guard n) = (n, False, -1, dat)
    updateLogs (guard, False, -1, dat) (time, Sleep) =
      (guard, True, minute time, dat)
    updateLogs (guard, True, timeFall, dat) (time, Wake) =
      ( guard
      , False
      , -1
      , Map.insertWith
          (Map.union)
          guard
          (Map.singleton (getDate time) [timeFall .. ((minute time) - 1)])
          dat)

getMinutesSlept :: SleepTimes -> Int
getMinutesSlept = length . concat . Map.elems

counts :: (Ord a) => [a] -> [(a,Int)]
counts = map (\ l -> (head l, length l)) . group . sort

groupByDate :: [LogEntry] -> Map Date [LogEntry]
groupByDate = foldl (\ m l@(t,_) -> Map.insertWith (++) (getDate t) [l] m) mempty

getDate :: TimeStamp -> Date
getDate Time{..} = (year,month,day)

getTime :: TimeStamp -> Time
getTime Time{..} = (hour,minute)

timestampParser :: Parser TimeStamp
timestampParser = do
  char '['
  year <- read <$> many1 digit
  char '-'
  month <- read <$> many1 digit
  char '-'
  day <- read <$> many1 digit
  space
  hour <- read <$> many1 digit
  char ':'
  minute <- read <$> many1 digit
  char ']'
  return Time{..}

actionParser :: Parser Action
actionParser = choice [parseGuard, parseSleep, parseWake]
  where
    parseGuard = do
      string "Guard #"
      num <- read <$> many1 digit
      string " begins shift"
      return $ Guard num

    parseSleep = do
      string "falls asleep"
      return Sleep

    parseWake = do
      string "wakes up"
      return Wake

type LogEntry = (TimeStamp,Action)

logEntryParser :: Parser LogEntry
logEntryParser = do
  time <- timestampParser
  spaces
  action <- actionParser
  return (time, action)

parseLogEntry :: String -> LogEntry
parseLogEntry s = case parse logEntryParser "STDIN" s of
  Left err -> error $ show err
  Right log -> log
