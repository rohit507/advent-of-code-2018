module Day4 where


import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

import Debug.Trace

import Text.Show.Pretty

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
part2 = interact $ ppShow . go . map parseLogEntry . sort . lines
  where
    go l = let dat = getSleepTimes l
               guardSleeps = fmap getSleepiestMin' dat
               maxGrdMin (g,(m,s)) (g',(m',s')) = if s' > s then (g',(m',s')) else (g,(m,s))
               (grd,(min,x)) = foldl maxGrdMin (-1,(-1,-1)) (Map.assocs guardSleeps)
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
type GuardData = Map Guard [Min]
type SleepHist = Map Min Int

type SleepState = (Guard, IsAsleep, FellAsleep, GuardData)

getSleepiestGuard :: GuardData -> Guard
getSleepiestGuard = fst . foldl maxSleep (-1,-1) . Map.assocs . fmap length
  where
    maxSleep (n,s) (n', s') = if s' > s then (n',s') else (n,s)

getSleepiestMin :: [Min] -> Min
getSleepiestMin = fst . getSleepiestMin'

getSleepiestMin' :: [Min] -> (Min,Int)
getSleepiestMin' = foldl maxMinute (-1,-1) . counts
  where
    maxMinute (n,s) (n', s') = if s' >= s then (n',s') else (n,s)

getSleepTimes :: [LogEntry] -> GuardData
getSleepTimes logs =
  let (_, _, _, d) = foldl updateLogs startState logs
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
    updateLogs (_, False, -1, dat) (_, Guard n) = (n, False, -1, dat)
    updateLogs (guard, False, -1, dat) (time, Sleep) =
      (guard, True, minute time, dat)
    updateLogs (guard, True, timeFall, dat) (time, Wake) =
      ( guard
      , False
      , -1
      , Map.insertWith
          (++)
          guard
          [timeFall .. ((minute time) - 1)]
          dat)
    updateLogs _ _ = undefined

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

{-
  fromList [
(G {_gId = 73},(finite 29,13)),
(G {_gId = 401},(finite 44,12)),(
G {_gId = 433},(finite 49,9)),
(G {_gId = 509},(finite 25,20)),
(G {_gId = 619},(finite 39,13)),
(G {_gId = 1301},(finite 48,13)),
(G {_gId = 1327},(finite 43,4)),
(G {_gId = 1409},(finite 44,11)),
(G {_gId = 1831},(finite 38,11)),
(G {_gId = 1877},(finite 38,9)),
(G {_gId = 2207},(finite 48,11)),
(G {_gId = 2411},(finite 39,8)),
(G {_gId = 2417},(finite 42,10)),
(G {_gId = 2459},(finite 46,6)),
(G {_gId = 2663},(finite 45,15)),
(G {_gId = 2861},(finite 33,10)),
(G {_gId = 3271},(finite 45,14)),
(G {_gId = 3469},(finite 24,7)),
(G {_gId = 3499},(finite 48,17)),
(G {_gId = 3541},(finite 35,13))]
[1518-01-29 23:57] Guard #2417 begins shift
[1518-01-30 00:13] falls asleep
[1518-01-30 00:50] wakes up
[1518-01-31 00:00] Guard #1301 begins shift
[1518-01-31 00:24] falls asleep
[1518-01-31 00:54] wakes up
[1518-01-31 23:59] Guard #401 begins shift
[1518-02-01 00:42] falls asleep
[1518-02-01 00:51] wakes up
[1518-02-02 00:03] Guard #2207 begins shift
[1518-02-02 00:10] falls asleep
[1518-02-02 00:49] wakes up
[1518-02-03 00:03] Guard #3541 begins shift
[1518-02-03 00:22] falls asleep
[1518-02-03 00:56] wakes up
[1518-02-04 00:03] Guard #2207 begins shift
[1518-02-04 00:09] falls asleep
[1518-02-04 00:26] wakes up
[1518-02-04 00:42] falls asleep
[1518-02-04 00:58] wakes up
[1518-02-05 00:00] Guard #2663 begins shift
[1518-02-05 00:44] falls asleep
[1518-02-05 00:50] wakes up
[1518-02-05 23:57] Guard #2207 begins shift
[1518-02-06 00:54] falls asleep
[1518-02-06 00:59] wakes up
[1518-02-07 00:00] Guard #1301 begins shift
[1518-02-07 00:44] falls asleep
[1518-02-07 00:53] wakes up
[1518-02-08 00:03] Guard #1327 begins shift
[1518-02-08 00:07] falls asleep
[1518-02-08 00:22] wakes up
[1518-02-08 00:31] falls asleep
[1518-02-08 00:51] wakes up
[1518-02-08 23:58] Guard #2411 begins shift
[1518-02-09 00:34] falls asleep
[1518-02-09 00:50] wakes up
[1518-02-10 00:00] Guard #2411 begins shift
[1518-02-10 00:42] falls asleep
[1518-02-10 00:49] wakes up
[1518-02-10 23:47] Guard #3541 begins shift
[1518-02-11 00:05] falls asleep
[1518-02-11 00:18] wakes up
[1518-02-12 00:01] Guard #1831 begins shift
[1518-02-12 00:17] falls asleep
[1518-02-12 00:35] wakes up
[1518-02-12 00:38] falls asleep
[1518-02-12 00:51] wakes up
[1518-02-13 00:02] Guard #1301 begins shift
[1518-02-13 00:10] falls asleep
[1518-02-13 00:55] wakes up
[1518-02-14 00:00] Guard #2459 begins shift
[1518-02-14 00:35] falls asleep
[1518-02-14 00:48] wakes up
[1518-02-14 00:52] falls asleep
[1518-02-14 00:57] wakes up
[1518-02-15 00:03] Guard #3499 begins shift
[1518-02-15 00:46] falls asleep
[1518-02-15 00:58] wakes up
[1518-02-16 00:04] Guard #2459 begins shift
[1518-02-16 00:13] falls asleep
[1518-02-16 00:26] wakes up
[1518-02-16 00:39] falls asleep
[1518-02-16 00:41] wakes up
[1518-02-16 00:45] falls asleep
[1518-02-16 00:59] wakes up
[1518-02-16 23:59] Guard #1327 begins shift
[1518-02-17 00:13] falls asleep
[1518-02-17 00:46] wakes up
[1518-02-17 00:57] falls asleep
[1518-02-17 00:59] wakes up
[1518-02-17 23:47] Guard #2417 begins shift
[1518-02-18 00:01] falls asleep
[1518-02-18 00:53] wakes up
[1518-02-18 23:59] Guard #2663 begins shift
[1518-02-19 00:27] falls asleep
[1518-02-19 00:37] wakes up
[1518-02-19 00:52] falls asleep
[1518-02-19 00:55] wakes up
[1518-02-20 00:02] Guard #3191 begins shift
[1518-02-21 00:00] Guard #3469 begins shift
[1518-02-21 00:16] falls asleep
[1518-02-21 00:26] wakes up
[1518-02-21 00:36] falls asleep
[1518-02-21 00:49] wakes up
[1518-02-21 00:55] falls asleep
[1518-02-21 00:56] wakes up
[1518-02-21 23:57] Guard #1409 begins shift
[1518-02-22 00:14] falls asleep
[1518-02-22 00:57] wakes up
[1518-02-23 00:00] Guard #2207 begins shift
[1518-02-23 00:11] falls asleep
[1518-02-23 00:19] wakes up
[1518-02-24 00:00] Guard #3271 begins shift
[1518-02-24 00:19] falls asleep
[1518-02-24 00:50] wakes up
[1518-02-25 00:02] Guard #1877 begins shift
[1518-02-25 00:22] falls asleep
[1518-02-25 00:42] wakes up
[1518-02-25 00:46] falls asleep
[1518-02-25 00:49] wakes up
[1518-02-26 00:04] Guard #619 begins shift
[1518-02-26 00:08] falls asleep
[1518-02-26 00:42] wakes up
[1518-02-26 23:58] Guard #1301 begins shift
[1518-02-27 00:20] falls asleep
[1518-02-27 00:32] wakes up
[1518-02-27 00:43] falls asleep
[1518-02-27 00:57] wakes up
[1518-02-27 23:59] Guard #2861 begins shift
[1518-02-28 00:31] falls asleep
[1518-02-28 00:38] wakes up
[1518-02-28 00:43] falls asleep
[1518-02-28 00:56] wakes up
[1518-03-01 00:00] Guard #2411 begins shift
[1518-03-01 00:26] falls asleep
[1518-03-01 00:34] wakes up
[1518-03-02 00:00] Guard #2207 begins shift
[1518-03-02 00:56] falls asleep
[1518-03-02 00:58] wakes up
[1518-03-03 00:00] Guard #433 begins shift
[1518-03-03 00:06] falls asleep
[1518-03-03 00:08] wakes up
[1518-03-03 23:54] Guard #1327 begins shift
[1518-03-04 00:02] falls asleep
[1518-03-04 00:44] wakes up
[1518-03-05 00:03] Guard #401 begins shift
[1518-03-05 00:45] falls asleep
[1518-03-05 00:46] wakes up
[1518-03-05 23:46] Guard #401 begins shift
[1518-03-06 00:02] falls asleep
[1518-03-06 00:57] wakes up
[1518-03-06 23:53] Guard #3469 begins shift
[1518-03-07 00:00] falls asleep
[1518-03-07 00:05] wakes up
[1518-03-07 00:36] falls asleep
[1518-03-07 00:43] wakes up
[1518-03-07 00:46] falls asleep
[1518-03-07 00:50] wakes up
[1518-03-08 00:02] Guard #619 begins shift
[1518-03-08 00:22] falls asleep
[1518-03-08 00:24] wakes up
[1518-03-08 00:30] falls asleep
[1518-03-08 00:48] wakes up
[1518-03-09 00:00] Guard #1831 begins shift
[1518-03-09 00:30] falls asleep
[1518-03-09 00:58] wakes up
[1518-03-09 23:56] Guard #2333 begins shift
[1518-03-10 23:58] Guard #509 begins shift
[1518-03-11 00:09] falls asleep
[1518-03-11 00:30] wakes up
[1518-03-11 00:34] falls asleep
[1518-03-11 00:49] wakes up
[1518-03-12 00:01] Guard #2663 begins shift
[1518-03-12 00:38] falls asleep
[1518-03-12 00:46] wakes up
[1518-03-12 00:49] falls asleep
[1518-03-12 00:52] wakes up
[1518-03-13 00:02] Guard #2663 begins shift
[1518-03-13 00:13] falls asleep
[1518-03-13 00:56] wakes up
[1518-03-14 00:00] Guard #2663 begins shift
[1518-03-14 00:06] falls asleep
[1518-03-14 00:47] wakes up
[1518-03-14 23:59] Guard #2861 begins shift
[1518-03-15 00:18] falls asleep
[1518-03-15 00:33] wakes up
[1518-03-15 00:42] falls asleep
[1518-03-15 00:47] wakes up
[1518-03-15 23:59] Guard #2459 begins shift
[1518-03-16 00:31] falls asleep
[1518-03-16 00:35] wakes up
[1518-03-16 00:43] falls asleep
[1518-03-16 00:48] wakes up
[1518-03-16 00:51] falls asleep
[1518-03-16 00:53] wakes up
[1518-03-16 23:58] Guard #1409 begins shift
[1518-03-17 00:06] falls asleep
[1518-03-17 00:46] wakes up
[1518-03-18 00:03] Guard #1409 begins shift
[1518-03-18 00:19] falls asleep
[1518-03-18 00:29] wakes up
[1518-03-18 00:40] falls asleep
[1518-03-18 00:56] wakes up
[1518-03-18 23:57] Guard #1301 begins shift
[1518-03-19 00:33] falls asleep
[1518-03-19 00:34] wakes up
[1518-03-19 23:59] Guard #2459 begins shift
[1518-03-20 00:23] falls asleep
[1518-03-20 00:51] wakes up
[1518-03-21 00:04] Guard #1831 begins shift
[1518-03-21 00:09] falls asleep
[1518-03-21 00:16] wakes up
[1518-03-21 00:45] falls asleep
[1518-03-21 00:56] wakes up
[1518-03-22 00:02] Guard #619 begins shift
[1518-03-22 00:20] falls asleep
[1518-03-22 00:40] wakes up
[1518-03-22 00:43] falls asleep
[1518-03-22 00:52] wakes up
[1518-03-22 00:56] falls asleep
[1518-03-22 00:59] wakes up
[1518-03-23 00:04] Guard #509 begins shift
[1518-03-23 00:21] falls asleep
[1518-03-23 00:26] wakes up
[1518-03-23 00:30] falls asleep
[1518-03-23 00:48] wakes up
[1518-03-23 23:48] Guard #3271 begins shift
[1518-03-24 00:01] falls asleep
[1518-03-24 00:16] wakes up
[1518-03-24 00:26] falls asleep
[1518-03-24 00:30] wakes up
[1518-03-24 23:54] Guard #3469 begins shift
[1518-03-25 00:01] falls asleep
[1518-03-25 00:23] wakes up
[1518-03-25 00:31] falls asleep
[1518-03-25 00:52] wakes up
[1518-03-26 00:03] Guard #2207 begins shift
[1518-03-26 00:35] falls asleep
[1518-03-26 00:54] wakes up
[1518-03-27 00:01] Guard #509 begins shift
[1518-03-27 00:09] falls asleep
[1518-03-27 00:29] wakes up
[1518-03-27 00:32] falls asleep
[1518-03-27 00:56] wakes up
[1518-03-27 23:56] Guard #2459 begins shift
[1518-03-28 00:22] falls asleep
[1518-03-28 00:27] wakes up
[1518-03-28 00:43] falls asleep
[1518-03-28 00:47] wakes up
[1518-03-28 23:57] Guard #2207 begins shift
[1518-03-29 00:39] falls asleep
[1518-03-29 00:46] wakes up
[1518-03-29 23:52] Guard #401 begins shift
[1518-03-30 00:05] falls asleep
[1518-03-30 00:55] wakes up
[1518-03-31 00:03] Guard #2663 begins shift
[1518-03-31 00:09] falls asleep
[1518-03-31 00:46] wakes up
[1518-04-01 00:00] Guard #2861 begins shift
[1518-04-01 00:09] falls asleep
[1518-04-01 00:59] wakes up
[1518-04-01 23:58] Guard #1877 begins shift
[1518-04-02 00:10] falls asleep
[1518-04-02 00:48] wakes up
[1518-04-02 23:56] Guard #3271 begins shift
[1518-04-03 00:29] falls asleep
[1518-04-03 00:46] wakes up
[1518-04-03 00:49] falls asleep
[1518-04-03 00:56] wakes up
[1518-04-03 23:59] Guard #619 begins shift
[1518-04-04 00:30] falls asleep
[1518-04-04 00:40] wakes up
[1518-04-04 00:50] falls asleep
[1518-04-04 00:51] wakes up
[1518-04-04 23:54] Guard #2663 begins shift
[1518-04-05 00:03] falls asleep
[1518-04-05 00:55] wakes up
[1518-04-06 00:01] Guard #3271 begins shift
[1518-04-06 00:44] falls asleep
[1518-04-06 00:57] wakes up
[1518-04-06 23:58] Guard #433 begins shift
[1518-04-07 00:16] falls asleep
[1518-04-07 00:46] wakes up
[1518-04-07 00:49] falls asleep
[1518-04-07 00:50] wakes up
[1518-04-07 00:53] falls asleep
[1518-04-07 00:57] wakes up
[1518-04-08 00:00] Guard #2663 begins shift
[1518-04-08 00:26] falls asleep
[1518-04-08 00:28] wakes up
[1518-04-08 00:45] falls asleep
[1518-04-08 00:54] wakes up
[1518-04-09 00:04] Guard #1697 begins shift
[1518-04-09 23:58] Guard #2861 begins shift
[1518-04-10 00:26] falls asleep
[1518-04-10 00:44] wakes up
[1518-04-10 23:49] Guard #73 begins shift
[1518-04-11 00:05] falls asleep
[1518-04-11 00:24] wakes up
[1518-04-11 00:28] falls asleep
[1518-04-11 00:35] wakes up
[1518-04-11 00:49] falls asleep
[1518-04-11 00:52] wakes up
[1518-04-11 00:57] falls asleep
[1518-04-11 00:59] wakes up
[1518-04-11 23:59] Guard #1697 begins shift
[1518-04-12 23:58] Guard #3191 begins shift
[1518-04-14 00:04] Guard #3469 begins shift
[1518-04-14 00:29] falls asleep
[1518-04-14 00:38] wakes up
[1518-04-15 00:00] Guard #1831 begins shift
[1518-04-15 00:35] falls asleep
[1518-04-15 00:39] wakes up
[1518-04-15 00:52] falls asleep
[1518-04-15 00:53] wakes up
[1518-04-16 00:00] Guard #2207 begins shift
[1518-04-16 00:24] falls asleep
[1518-04-16 00:59] wakes up
[1518-04-16 23:57] Guard #2861 begins shift
[1518-04-17 00:22] falls asleep
[1518-04-17 00:34] wakes up
[1518-04-17 00:41] falls asleep
[1518-04-17 00:53] wakes up
[1518-04-18 00:01] Guard #401 begins shift
[1518-04-18 00:31] falls asleep
[1518-04-18 00:59] wakes up
[1518-04-19 00:00] Guard #1409 begins shift
[1518-04-19 00:46] falls asleep
[1518-04-19 00:51] wakes up
[1518-04-19 23:47] Guard #619 begins shift
[1518-04-20 00:01] falls asleep
[1518-04-20 00:42] wakes up
[1518-04-20 23:58] Guard #2663 begins shift
[1518-04-21 00:12] falls asleep
[1518-04-21 00:52] wakes up
[1518-04-22 00:02] Guard #2207 begins shift
[1518-04-22 00:47] falls asleep
[1518-04-22 00:53] wakes up
[1518-04-23 00:00] Guard #1327 begins shift
[1518-04-23 00:40] falls asleep
[1518-04-23 00:53] wakes up
[1518-04-23 00:57] falls asleep
[1518-04-23 00:58] wakes up
[1518-04-24 00:01] Guard #2207 begins shift
[1518-04-24 00:24] falls asleep
[1518-04-24 00:43] wakes up
[1518-04-25 00:00] Guard #1877 begins shift
[1518-04-25 00:37] falls asleep
[1518-04-25 00:49] wakes up
[1518-04-25 23:54] Guard #1301 begins shift
[1518-04-26 00:00] falls asleep
[1518-04-26 00:04] wakes up
[1518-04-26 00:14] falls asleep
[1518-04-26 00:29] wakes up
[1518-04-26 00:46] falls asleep
[1518-04-26 00:49] wakes up
[1518-04-27 00:03] Guard #2333 begins shift
[1518-04-28 00:01] Guard #3541 begins shift
[1518-04-28 00:27] falls asleep
[1518-04-28 00:30] wakes up
[1518-04-28 00:33] falls asleep
[1518-04-28 00:59] wakes up
[1518-04-28 23:48] Guard #2663 begins shift
[1518-04-29 00:02] falls asleep
[1518-04-29 00:26] wakes up
[1518-04-30 00:01] Guard #3499 begins shift
[1518-04-30 00:28] falls asleep
[1518-04-30 00:31] wakes up
[1518-04-30 00:47] falls asleep
[1518-04-30 00:58] wakes up
[1518-05-01 00:02] Guard #3271 begins shift
[1518-05-01 00:12] falls asleep
[1518-05-01 00:48] wakes up
[1518-05-02 00:04] Guard #1409 begins shift
[1518-05-02 00:33] falls asleep
[1518-05-02 00:48] wakes up
[1518-05-02 00:52] falls asleep
[1518-05-02 00:58] wakes up
[1518-05-02 23:57] Guard #1301 begins shift
[1518-05-03 00:12] falls asleep
[1518-05-03 00:51] wakes up
[1518-05-03 23:59] Guard #1831 begins shift
[1518-05-04 00:10] falls asleep
[1518-05-04 00:58] wakes up
[1518-05-04 23:50] Guard #2207 begins shift
[1518-05-05 00:05] falls asleep
[1518-05-05 00:11] wakes up
[1518-05-05 00:23] falls asleep
[1518-05-05 00:50] wakes up
[1518-05-05 23:58] Guard #3271 begins shift
[1518-05-06 00:43] falls asleep
[1518-05-06 00:51] wakes up
[1518-05-07 00:02] Guard #73 begins shift
[1518-05-07 00:17] falls asleep
[1518-05-07 00:54] wakes up
[1518-05-08 00:00] Guard #2411 begins shift
[1518-05-08 00:06] falls asleep
[1518-05-08 00:18] wakes up
[1518-05-08 00:50] falls asleep
[1518-05-08 00:59] wakes up
[1518-05-08 23:59] Guard #2861 begins shift
[1518-05-09 00:07] falls asleep
[1518-05-09 00:41] wakes up
[1518-05-09 23:49] Guard #3541 begins shift
[1518-05-10 00:03] falls asleep
[1518-05-10 00:59] wakes up
[1518-05-11 00:01] Guard #3469 begins shift
[1518-05-11 00:16] falls asleep
[1518-05-11 00:26] wakes up
[1518-05-11 00:52] falls asleep
[1518-05-11 00:54] wakes up
[1518-05-12 00:00] Guard #1301 begins shift
[1518-05-12 00:32] falls asleep
[1518-05-12 00:38] wakes up
[1518-05-12 00:44] falls asleep
[1518-05-12 00:53] wakes up
[1518-05-13 00:00] Guard #3499 begins shift
[1518-05-13 00:40] falls asleep
[1518-05-13 00:50] wakes up
[1518-05-13 23:59] Guard #509 begins shift
[1518-05-14 00:25] falls asleep
[1518-05-14 00:48] wakes up
[1518-05-15 00:04] Guard #1301 begins shift
[1518-05-15 00:10] falls asleep
[1518-05-15 00:53] wakes up
[1518-05-15 23:56] Guard #3499 begins shift
[1518-05-16 00:14] falls asleep
[1518-05-16 00:31] wakes up
[1518-05-16 00:36] falls asleep
[1518-05-16 00:51] wakes up
[1518-05-17 00:04] Guard #2411 begins shift
[1518-05-17 00:33] falls asleep
[1518-05-17 00:42] wakes up
[1518-05-17 00:46] falls asleep
[1518-05-17 00:55] wakes up
[1518-05-18 00:04] Guard #2417 begins shift
[1518-05-18 00:38] falls asleep
[1518-05-18 00:48] wakes up
[1518-05-18 00:56] falls asleep
[1518-05-18 00:59] wakes up
[1518-05-19 00:02] Guard #2411 begins shift
[1518-05-19 00:35] falls asleep
[1518-05-19 00:40] wakes up
[1518-05-19 23:57] Guard #3541 begins shift
[1518-05-20 00:25] falls asleep
[1518-05-20 00:47] wakes up
[1518-05-20 23:56] Guard #1301 begins shift
[1518-05-21 00:10] falls asleep
[1518-05-21 00:49] wakes up
[1518-05-21 23:58] Guard #509 begins shift
[1518-05-22 00:22] falls asleep
[1518-05-22 00:31] wakes up
[1518-05-23 00:04] Guard #401 begins shift
[1518-05-23 00:29] falls asleep
[1518-05-23 00:37] wakes up
[1518-05-24 00:00] Guard #509 begins shift
[1518-05-24 00:23] falls asleep
[1518-05-24 00:28] wakes up
[1518-05-24 00:57] falls asleep
[1518-05-24 00:58] wakes up
[1518-05-24 23:50] Guard #3271 begins shift
[1518-05-25 00:05] falls asleep
[1518-05-25 00:56] wakes up
[1518-05-26 00:03] Guard #2411 begins shift
[1518-05-26 00:14] falls asleep
[1518-05-26 00:17] wakes up
[1518-05-26 00:21] falls asleep
[1518-05-26 00:48] wakes up
[1518-05-26 23:49] Guard #3271 begins shift
[1518-05-27 00:04] falls asleep
[1518-05-27 00:12] wakes up
[1518-05-28 00:00] Guard #2417 begins shift
[1518-05-28 00:07] falls asleep
[1518-05-28 00:44] wakes up
[1518-05-28 23:57] Guard #401 begins shift
[1518-05-29 00:18] falls asleep
[1518-05-29 00:45] wakes up
[1518-05-29 00:53] falls asleep
[1518-05-29 00:58] wakes up
[1518-05-30 00:02] Guard #3499 begins shift
[1518-05-30 00:20] falls asleep
[1518-05-30 00:42] wakes up
[1518-05-30 00:48] falls asleep
[1518-05-30 00:51] wakes up
[1518-05-30 23:56] Guard #2861 begins shift
[1518-05-31 00:32] falls asleep
[1518-05-31 00:39] wakes up
[1518-06-01 00:03] Guard #2417 begins shift
[1518-06-01 00:25] falls asleep
[1518-06-01 00:57] wakes up
[1518-06-02 00:04] Guard #1301 begins shift
[1518-06-02 00:27] falls asleep
[1518-06-02 00:36] wakes up
[1518-06-02 00:42] falls asleep
[1518-06-02 00:54] wakes up
[1518-06-03 00:01] Guard #3469 begins shift
[1518-06-03 00:18] falls asleep
[1518-06-03 00:27] wakes up
[1518-06-03 23:58] Guard #509 begins shift
[1518-06-04 00:19] falls asleep
[1518-06-04 00:29] wakes up
[1518-06-04 00:48] falls asleep
[1518-06-04 00:58] wakes up
[1518-06-05 00:00] Guard #619 begins shift
[1518-06-05 00:33] falls asleep
[1518-06-05 00:45] wakes up
[1518-06-05 23:59] Guard #509 begins shift
[1518-06-06 00:25] falls asleep
[1518-06-06 00:36] wakes up
[1518-06-06 23:56] Guard #3271 begins shift
[1518-06-07 00:41] falls asleep
[1518-06-07 00:57] wakes up
[1518-06-08 00:04] Guard #2207 begins shift
[1518-06-08 00:38] falls asleep
[1518-06-08 00:47] wakes up
[1518-06-09 00:01] Guard #1831 begins shift
[1518-06-09 00:16] falls asleep
[1518-06-09 00:41] wakes up
[1518-06-09 23:56] Guard #3271 begins shift
[1518-06-10 00:10] falls asleep
[1518-06-10 00:51] wakes up
[1518-06-10 23:59] Guard #433 begins shift
[1518-06-11 00:19] falls asleep
[1518-06-11 00:26] wakes up
[1518-06-11 00:31] falls asleep
[1518-06-11 00:54] wakes up
[1518-06-12 00:00] Guard #2861 begins shift
[1518-06-12 00:33] falls asleep
[1518-06-12 00:47] wakes up
[1518-06-12 00:50] falls asleep
[1518-06-12 00:59] wakes up
[1518-06-12 23:54] Guard #73 begins shift
[1518-06-13 00:01] falls asleep
[1518-06-13 00:41] wakes up
[1518-06-13 23:53] Guard #73 begins shift
[1518-06-14 00:04] falls asleep
[1518-06-14 00:58] wakes up
[1518-06-15 00:00] Guard #1409 begins shift
[1518-06-15 00:06] falls asleep
[1518-06-15 00:20] wakes up
[1518-06-15 00:44] falls asleep
[1518-06-15 00:47] wakes up
[1518-06-16 00:03] Guard #3271 begins shift
[1518-06-16 00:48] falls asleep
[1518-06-16 00:56] wakes up
[1518-06-17 00:03] Guard #3499 begins shift
[1518-06-17 00:22] falls asleep
[1518-06-17 00:53] wakes up
[1518-06-17 00:57] falls asleep
[1518-06-17 00:58] wakes up
[1518-06-17 23:50] Guard #401 begins shift
[1518-06-18 00:02] falls asleep
[1518-06-18 00:49] wakes up
[1518-06-18 23:57] Guard #2411 begins shift
[1518-06-19 00:21] falls asleep
[1518-06-19 00:24] wakes up
[1518-06-19 23:57] Guard #509 begins shift
[1518-06-20 00:16] falls asleep
[1518-06-20 00:54] wakes up
[1518-06-20 23:58] Guard #619 begins shift
[1518-06-21 00:31] falls asleep
[1518-06-21 00:59] wakes up
[1518-06-22 00:02] Guard #619 begins shift
[1518-06-22 00:06] falls asleep
[1518-06-22 00:52] wakes up
[1518-06-22 23:57] Guard #619 begins shift
[1518-06-23 00:10] falls asleep
[1518-06-23 00:28] wakes up
[1518-06-23 00:32] falls asleep
[1518-06-23 00:41] wakes up
[1518-06-23 00:54] falls asleep
[1518-06-23 00:59] wakes up
[1518-06-24 00:03] Guard #3541 begins shift
[1518-06-24 00:28] falls asleep
[1518-06-24 00:42] wakes up
[1518-06-24 23:49] Guard #3271 begins shift
[1518-06-25 00:03] falls asleep
[1518-06-25 00:47] wakes up
[1518-06-26 00:03] Guard #2459 begins shift
[1518-06-26 00:28] falls asleep
[1518-06-26 00:57] wakes up
[1518-06-27 00:01] Guard #1877 begins shift
[1518-06-27 00:30] falls asleep
[1518-06-27 00:44] wakes up
[1518-06-27 00:47] falls asleep
[1518-06-27 00:48] wakes up
[1518-06-28 00:00] Guard #433 begins shift
[1518-06-28 00:25] falls asleep
[1518-06-28 00:57] wakes up
[1518-06-28 23:57] Guard #1327 begins shift
[1518-06-29 00:07] falls asleep
[1518-06-29 00:11] wakes up
[1518-06-29 23:59] Guard #1409 begins shift
[1518-06-30 00:18] falls asleep
[1518-06-30 00:24] wakes up
[1518-06-30 00:40] falls asleep
[1518-06-30 00:57] wakes up
[1518-07-01 00:01] Guard #73 begins shift
[1518-07-01 00:37] falls asleep
[1518-07-01 00:47] wakes up
[1518-07-01 23:59] Guard #1301 begins shift
[1518-07-02 00:06] falls asleep
[1518-07-02 00:48] wakes up
[1518-07-03 00:02] Guard #73 begins shift
[1518-07-03 00:19] falls asleep
[1518-07-03 00:58] wakes up
[1518-07-04 00:01] Guard #2417 begins shift
[1518-07-04 00:22] falls asleep
[1518-07-04 00:38] wakes up
[1518-07-04 00:54] falls asleep
[1518-07-04 00:59] wakes up
[1518-07-05 00:01] Guard #2663 begins shift
[1518-07-05 00:40] falls asleep
[1518-07-05 00:48] wakes up
[1518-07-05 23:51] Guard #1327 begins shift
[1518-07-06 00:01] falls asleep
[1518-07-06 00:24] wakes up
[1518-07-06 00:48] falls asleep
[1518-07-06 00:55] wakes up
[1518-07-07 00:02] Guard #1877 begins shift
[1518-07-07 00:35] falls asleep
[1518-07-07 00:52] wakes up
[1518-07-08 00:04] Guard #3541 begins shift
[1518-07-08 00:11] falls asleep
[1518-07-08 00:36] wakes up
[1518-07-08 23:57] Guard #619 begins shift
[1518-07-09 00:42] falls asleep
[1518-07-09 00:56] wakes up
[1518-07-10 00:00] Guard #2861 begins shift
[1518-07-10 00:10] falls asleep
[1518-07-10 00:11] wakes up
[1518-07-10 00:23] falls asleep
[1518-07-10 00:26] wakes up
[1518-07-11 00:01] Guard #619 begins shift
[1518-07-11 00:28] falls asleep
[1518-07-11 00:56] wakes up
[1518-07-11 23:47] Guard #3469 begins shift
[1518-07-12 00:04] falls asleep
[1518-07-12 00:08] wakes up
[1518-07-12 00:17] falls asleep
[1518-07-12 00:37] wakes up
[1518-07-13 00:00] Guard #1831 begins shift
[1518-07-13 00:36] falls asleep
[1518-07-13 00:39] wakes up
[1518-07-13 00:43] falls asleep
[1518-07-13 00:58] wakes up
[1518-07-13 23:56] Guard #401 begins shift
[1518-07-14 00:30] falls asleep
[1518-07-14 00:46] wakes up
[1518-07-14 00:54] falls asleep
[1518-07-14 00:57] wakes up
[1518-07-14 23:59] Guard #3469 begins shift
[1518-07-15 00:24] falls asleep
[1518-07-15 00:27] wakes up
[1518-07-16 00:00] Guard #509 begins shift
[1518-07-16 00:24] falls asleep
[1518-07-16 00:29] wakes up
[1518-07-17 00:00] Guard #1409 begins shift
[1518-07-17 00:12] falls asleep
[1518-07-17 00:56] wakes up
[1518-07-17 23:57] Guard #1301 begins shift
[1518-07-18 00:35] falls asleep
[1518-07-18 00:55] wakes up
[1518-07-18 23:54] Guard #2411 begins shift
[1518-07-19 00:04] falls asleep
[1518-07-19 00:19] wakes up
[1518-07-19 00:37] falls asleep
[1518-07-19 00:55] wakes up
[1518-07-20 00:03] Guard #433 begins shift
[1518-07-20 00:41] falls asleep
[1518-07-20 00:55] wakes up
[1518-07-21 00:01] Guard #2411 begins shift
[1518-07-21 00:50] falls asleep
[1518-07-21 00:54] wakes up
[1518-07-21 23:59] Guard #1409 begins shift
[1518-07-22 00:35] falls asleep
[1518-07-22 00:50] wakes up
[1518-07-22 23:56] Guard #2459 begins shift
[1518-07-23 00:26] falls asleep
[1518-07-23 00:45] wakes up
[1518-07-24 00:04] Guard #1409 begins shift
[1518-07-24 00:32] falls asleep
[1518-07-24 00:45] wakes up
[1518-07-25 00:01] Guard #509 begins shift
[1518-07-25 00:16] falls asleep
[1518-07-25 00:42] wakes up
[1518-07-25 23:56] Guard #433 begins shift
[1518-07-26 00:35] falls asleep
[1518-07-26 00:37] wakes up
[1518-07-26 00:46] falls asleep
[1518-07-26 00:58] wakes up
[1518-07-26 23:50] Guard #3541 begins shift
[1518-07-27 00:04] falls asleep
[1518-07-27 00:45] wakes up
[1518-07-27 00:55] falls asleep
[1518-07-27 00:59] wakes up
[1518-07-28 00:03] Guard #3499 begins shift
[1518-07-28 00:34] falls asleep
[1518-07-28 00:48] wakes up
[1518-07-28 23:58] Guard #619 begins shift
[1518-07-29 00:09] falls asleep
[1518-07-29 00:13] wakes up
[1518-07-29 00:19] falls asleep
[1518-07-29 00:53] wakes up
[1518-07-29 23:59] Guard #3469 begins shift
[1518-07-30 00:16] falls asleep
[1518-07-30 00:25] wakes up
[1518-07-30 00:38] falls asleep
[1518-07-30 00:48] wakes up
[1518-07-30 00:55] falls asleep
[1518-07-30 00:56] wakes up
[1518-07-31 00:02] Guard #401 begins shift
[1518-07-31 00:23] falls asleep
[1518-07-31 00:50] wakes up
[1518-08-01 00:03] Guard #3499 begins shift
[1518-08-01 00:40] falls asleep
[1518-08-01 00:58] wakes up
[1518-08-02 00:03] Guard #3469 begins shift
[1518-08-02 00:06] falls asleep
[1518-08-02 00:09] wakes up
[1518-08-02 00:14] falls asleep
[1518-08-02 00:40] wakes up
[1518-08-03 00:02] Guard #3191 begins shift
[1518-08-04 00:04] Guard #3541 begins shift
[1518-08-04 00:18] falls asleep
[1518-08-04 00:26] wakes up
[1518-08-04 00:35] falls asleep
[1518-08-04 00:37] wakes up
[1518-08-04 00:41] falls asleep
[1518-08-04 00:55] wakes up
[1518-08-05 00:03] Guard #509 begins shift
[1518-08-05 00:10] falls asleep
[1518-08-05 00:34] wakes up
[1518-08-06 00:00] Guard #3541 begins shift
[1518-08-06 00:17] falls asleep
[1518-08-06 00:27] wakes up
[1518-08-06 00:39] falls asleep
[1518-08-06 00:55] wakes up
[1518-08-07 00:02] Guard #2207 begins shift
[1518-08-07 00:47] falls asleep
[1518-08-07 00:54] wakes up
[1518-08-08 00:01] Guard #3541 begins shift
[1518-08-08 00:17] falls asleep
[1518-08-08 00:55] wakes up
[1518-08-08 23:58] Guard #2333 begins shift
[1518-08-10 00:00] Guard #2207 begins shift
[1518-08-10 00:21] falls asleep
[1518-08-10 00:33] wakes up
[1518-08-10 00:42] falls asleep
[1518-08-10 00:54] wakes up
[1518-08-10 23:57] Guard #1877 begins shift
[1518-08-11 00:16] falls asleep
[1518-08-11 00:51] wakes up
[1518-08-11 23:56] Guard #2411 begins shift
[1518-08-12 00:16] falls asleep
[1518-08-12 00:31] wakes up
[1518-08-12 23:59] Guard #619 begins shift
[1518-08-13 00:22] falls asleep
[1518-08-13 00:33] wakes up
[1518-08-14 00:00] Guard #2861 begins shift
[1518-08-14 00:25] falls asleep
[1518-08-14 00:35] wakes up
[1518-08-15 00:01] Guard #2411 begins shift
[1518-08-15 00:12] falls asleep
[1518-08-15 00:51] wakes up
[1518-08-15 00:54] falls asleep
[1518-08-15 00:58] wakes up
[1518-08-16 00:04] Guard #509 begins shift
[1518-08-16 00:25] falls asleep
[1518-08-16 00:30] wakes up
[1518-08-16 00:40] falls asleep
[1518-08-16 00:56] wakes up
[1518-08-16 23:56] Guard #3191 begins shift
[1518-08-18 00:01] Guard #401 begins shift
[1518-08-18 00:47] falls asleep
[1518-08-18 00:53] wakes up
[1518-08-19 00:00] Guard #2417 begins shift
[1518-08-19 00:07] falls asleep
[1518-08-19 00:50] wakes up
[1518-08-19 23:59] Guard #2663 begins shift
[1518-08-20 00:18] falls asleep
[1518-08-20 00:59] wakes up
[1518-08-20 23:54] Guard #1831 begins shift
[1518-08-21 00:01] falls asleep
[1518-08-21 00:55] wakes up
[1518-08-21 23:56] Guard #1301 begins shift
[1518-08-22 00:12] falls asleep
[1518-08-22 00:43] wakes up
[1518-08-22 00:46] falls asleep
[1518-08-22 00:59] wakes up
[1518-08-23 00:04] Guard #73 begins shift
[1518-08-23 00:11] falls asleep
[1518-08-23 00:56] wakes up
[1518-08-23 23:49] Guard #2207 begins shift
[1518-08-24 00:04] falls asleep
[1518-08-24 00:33] wakes up
[1518-08-24 00:40] falls asleep
[1518-08-24 00:56] wakes up
[1518-08-24 23:52] Guard #2663 begins shift
[1518-08-25 00:04] falls asleep
[1518-08-25 00:48] wakes up
[1518-08-25 23:57] Guard #73 begins shift
[1518-08-26 00:18] falls asleep
[1518-08-26 00:34] wakes up
[1518-08-26 00:44] falls asleep
[1518-08-26 00:52] wakes up
[1518-08-27 00:02] Guard #2663 begins shift
[1518-08-27 00:21] falls asleep
[1518-08-27 00:50] wakes up
[1518-08-28 00:04] Guard #1301 begins shift
[1518-08-28 00:16] falls asleep
[1518-08-28 00:42] wakes up
[1518-08-28 00:48] falls asleep
[1518-08-28 00:55] wakes up
[1518-08-28 23:57] Guard #401 begins shift
[1518-08-29 00:26] falls asleep
[1518-08-29 00:28] wakes up
[1518-08-29 00:34] falls asleep
[1518-08-29 00:45] wakes up
[1518-08-29 00:50] falls asleep
[1518-08-29 00:59] wakes up
[1518-08-29 23:52] Guard #2417 begins shift
[1518-08-30 00:04] falls asleep
[1518-08-30 00:29] wakes up
[1518-08-30 00:35] falls asleep
[1518-08-30 00:58] wakes up
[1518-08-30 23:57] Guard #1877 begins shift
[1518-08-31 00:48] falls asleep
[1518-08-31 00:55] wakes up
[1518-08-31 23:59] Guard #1831 begins shift
[1518-09-01 00:51] falls asleep
[1518-09-01 00:54] wakes up
[1518-09-01 23:57] Guard #3271 begins shift
[1518-09-02 00:08] falls asleep
[1518-09-02 00:36] wakes up
[1518-09-02 00:41] falls asleep
[1518-09-02 00:57] wakes up
[1518-09-02 23:59] Guard #3541 begins shift
[1518-09-03 00:25] falls asleep
[1518-09-03 00:42] wakes up
[1518-09-03 00:48] falls asleep
[1518-09-03 00:51] wakes up
[1518-09-03 23:57] Guard #2417 begins shift
[1518-09-04 00:34] falls asleep
[1518-09-04 00:47] wakes up
[1518-09-04 00:56] falls asleep
[1518-09-04 00:58] wakes up
[1518-09-05 00:00] Guard #3271 begins shift
[1518-09-05 00:06] falls asleep
[1518-09-05 00:56] wakes up
[1518-09-05 23:59] Guard #73 begins shift
[1518-09-06 00:37] falls asleep
[1518-09-06 00:46] wakes up
[1518-09-06 23:58] Guard #3499 begins shift
[1518-09-07 00:17] falls asleep
[1518-09-07 00:36] wakes up
[1518-09-07 00:45] falls asleep
[1518-09-07 00:58] wakes up
[1518-09-07 23:58] Guard #433 begins shift
[1518-09-08 00:47] falls asleep
[1518-09-08 00:54] wakes up
[1518-09-08 23:52] Guard #73 begins shift
[1518-09-09 00:02] falls asleep
[1518-09-09 00:43] wakes up
[1518-09-10 00:00] Guard #401 begins shift
[1518-09-10 00:16] falls asleep
[1518-09-10 00:26] wakes up
[1518-09-10 00:33] falls asleep
[1518-09-10 00:45] wakes up
[1518-09-10 00:52] falls asleep
[1518-09-10 00:58] wakes up
[1518-09-10 23:57] Guard #619 begins shift
[1518-09-11 00:09] falls asleep
[1518-09-11 00:59] wakes up
[1518-09-12 00:02] Guard #509 begins shift
[1518-09-12 00:22] falls asleep
[1518-09-12 00:27] wakes up
[1518-09-12 00:34] falls asleep
[1518-09-12 00:42] wakes up
[1518-09-12 00:56] falls asleep
[1518-09-12 00:57] wakes up
[1518-09-12 23:59] Guard #3541 begins shift
[1518-09-13 00:21] falls asleep
[1518-09-13 00:35] wakes up
[1518-09-13 23:58] Guard #1877 begins shift
[1518-09-14 00:25] falls asleep
[1518-09-14 00:54] wakes up
[1518-09-14 00:57] falls asleep
[1518-09-14 00:58] wakes up
[1518-09-14 23:59] Guard #2207 begins shift
[1518-09-15 00:44] falls asleep
[1518-09-15 00:51] wakes up
[1518-09-15 23:59] Guard #73 begins shift
[1518-09-16 00:28] falls asleep
[1518-09-16 00:35] wakes up
[1518-09-16 00:42] falls asleep
[1518-09-16 00:51] wakes up
[1518-09-17 00:03] Guard #1697 begins shift
[1518-09-18 00:03] Guard #3469 begins shift
[1518-09-18 00:27] falls asleep
[1518-09-18 00:34] wakes up
[1518-09-19 00:02] Guard #3499 begins shift
[1518-09-19 00:35] falls asleep
[1518-09-19 00:53] wakes up
[1518-09-20 00:03] Guard #433 begins shift
[1518-09-20 00:23] falls asleep
[1518-09-20 00:51] wakes up
[1518-09-20 23:58] Guard #1877 begins shift
[1518-09-21 00:13] falls asleep
[1518-09-21 00:57] wakes up
[1518-09-22 00:02] Guard #2663 begins shift
[1518-09-22 00:07] falls asleep
[1518-09-22 00:17] wakes up
[1518-09-22 00:24] falls asleep
[1518-09-22 00:27] wakes up
[1518-09-23 00:00] Guard #73 begins shift
[1518-09-23 00:11] falls asleep
[1518-09-23 00:46] wakes up
[1518-09-24 00:04] Guard #3499 begins shift
[1518-09-24 00:24] falls asleep
[1518-09-24 00:46] wakes up
[1518-09-24 23:56] Guard #2663 begins shift
[1518-09-25 00:28] falls asleep
[1518-09-25 00:34] wakes up
[1518-09-25 00:39] falls asleep
[1518-09-25 00:59] wakes up
[1518-09-26 00:01] Guard #2417 begins shift
[1518-09-26 00:08] falls asleep
[1518-09-26 00:34] wakes up
[1518-09-26 00:39] falls asleep
[1518-09-26 00:43] wakes up
[1518-09-27 00:00] Guard #2411 begins shift
[1518-09-27 00:15] falls asleep
[1518-09-27 00:35] wakes up
[1518-09-27 00:38] falls asleep
[1518-09-27 00:45] wakes up
[1518-09-28 00:03] Guard #1409 begins shift
[1518-09-28 00:09] falls asleep
[1518-09-28 00:27] wakes up
[1518-09-28 23:52] Guard #1831 begins shift
[1518-09-29 00:01] falls asleep
[1518-09-29 00:54] wakes up
[1518-09-29 23:59] Guard #433 begins shift
[1518-09-30 00:19] falls asleep
[1518-09-30 00:28] wakes up
[1518-09-30 00:32] falls asleep
[1518-09-30 00:42] wakes up
[1518-10-01 00:01] Guard #2207 begins shift
[1518-10-01 00:32] falls asleep
[1518-10-01 00:57] wakes up
[1518-10-01 23:59] Guard #3499 begins shift
[1518-10-02 00:32] falls asleep
[1518-10-02 00:34] wakes up
[1518-10-02 00:42] falls asleep
[1518-10-02 00:54] wakes up
[1518-10-03 00:00] Guard #2417 begins shift
[1518-10-03 00:36] falls asleep
[1518-10-03 00:50] wakes up
[1518-10-03 00:57] falls asleep
[1518-10-03 00:58] wakes up
[1518-10-03 23:59] Guard #2663 begins shift
[1518-10-04 00:47] falls asleep
[1518-10-04 00:48] wakes up
[1518-10-04 23:49] Guard #3271 begins shift
[1518-10-05 00:00] falls asleep
[1518-10-05 00:57] wakes up
[1518-10-05 23:57] Guard #73 begins shift
[1518-10-06 00:28] falls asleep
[1518-10-06 00:51] wakes up
[1518-10-07 00:00] Guard #1831 begins shift
[1518-10-07 00:18] falls asleep
[1518-10-07 00:44] wakes up
[1518-10-07 00:53] falls asleep
[1518-10-07 00:57] wakes up
[1518-10-07 23:56] Guard #619 begins shift
[1518-10-08 00:19] falls asleep
[1518-10-08 00:56] wakes up
[1518-10-09 00:00] Guard #509 begins shift
[1518-10-09 00:12] falls asleep
[1518-10-09 00:56] wakes up
[1518-10-10 00:04] Guard #1301 begins shift
[1518-10-10 00:07] falls asleep
[1518-10-10 00:39] wakes up
[1518-10-10 23:59] Guard #401 begins shift
[1518-10-11 00:37] falls asleep
[1518-10-11 00:40] wakes up
[1518-10-11 00:43] falls asleep
[1518-10-11 00:50] wakes up
[1518-10-12 00:00] Guard #509 begins shift
[1518-10-12 00:24] falls asleep
[1518-10-12 00:29] wakes up
[1518-10-12 00:52] falls asleep
[1518-10-12 00:56] wakes up
[1518-10-12 23:47] Guard #509 begins shift
[1518-10-13 00:04] falls asleep
[1518-10-13 00:54] wakes up
[1518-10-13 23:50] Guard #2207 begins shift
[1518-10-14 00:05] falls asleep
[1518-10-14 00:32] wakes up
[1518-10-14 00:55] falls asleep
[1518-10-14 00:57] wakes up
[1518-10-14 23:59] Guard #3541 begins shift
[1518-10-15 00:27] falls asleep
[1518-10-15 00:41] wakes up
[1518-10-16 00:02] Guard #3499 begins shift
[1518-10-16 00:26] falls asleep
[1518-10-16 00:39] wakes up
[1518-10-16 00:44] falls asleep
[1518-10-16 00:56] wakes up
[1518-10-17 00:04] Guard #1831 begins shift
[1518-10-17 00:13] falls asleep
[1518-10-17 00:48] wakes up
[1518-10-17 00:51] falls asleep
[1518-10-17 00:52] wakes up
[1518-10-18 00:04] Guard #509 begins shift
[1518-10-18 00:08] falls asleep
[1518-10-18 00:42] wakes up
[1518-10-19 00:00] Guard #1877 begins shift
[1518-10-19 00:09] falls asleep
[1518-10-19 00:39] wakes up
[1518-10-19 00:54] falls asleep
[1518-10-19 00:55] wakes up
[1518-10-19 23:56] Guard #433 begins shift
[1518-10-20 00:23] falls asleep
[1518-10-20 00:33] wakes up
[1518-10-20 00:53] falls asleep
[1518-10-20 00:57] wakes up
[1518-10-20 23:57] Guard #3499 begins shift
[1518-10-21 00:33] falls asleep
[1518-10-21 00:58] wakes up
[1518-10-22 00:00] Guard #3499 begins shift
[1518-10-22 00:32] falls asleep
[1518-10-22 00:49] wakes up
[1518-10-23 00:04] Guard #509 begins shift
[1518-10-23 00:11] falls asleep
[1518-10-23 00:52] wakes up
[1518-10-24 00:02] Guard #3541 begins shift
[1518-10-24 00:19] falls asleep
[1518-10-24 00:27] wakes up
[1518-10-24 00:35] falls asleep
[1518-10-24 00:42] wakes up
[1518-10-25 00:00] Guard #3499 begins shift
[1518-10-25 00:17] falls asleep
[1518-10-25 00:53] wakes up
[1518-10-25 23:52] Guard #73 begins shift
[1518-10-26 00:02] falls asleep
[1518-10-26 00:25] wakes up
[1518-10-26 00:35] falls asleep
[1518-10-26 00:36] wakes up
[1518-10-26 23:57] Guard #2411 begins shift
[1518-10-27 00:22] falls asleep
[1518-10-27 00:38] wakes up
[1518-10-27 23:57] Guard #2663 begins shift
[1518-10-28 00:31] falls asleep
[1518-10-28 00:52] wakes up
[1518-10-28 23:58] Guard #433 begins shift
[1518-10-29 00:28] falls asleep
[1518-10-29 00:52] wakes up
[1518-10-29 00:55] falls asleep
[1518-10-29 00:56] wakes up
[1518-10-30 00:03] Guard #1877 begins shift
[1518-10-30 00:14] falls asleep
[1518-10-30 00:35] wakes up
[1518-10-30 00:53] falls asleep
[1518-10-30 00:57] wakes up
[1518-10-31 00:01] Guard #3499 begins shift
[1518-10-31 00:28] falls asleep
[1518-10-31 00:58] wakes up
[1518-11-01 00:04] Guard #509 begins shift
[1518-11-01 00:08] falls asleep
[1518-11-01 00:45] wakes up
[1518-11-01 23:57] Guard #2207 begins shift
[1518-11-02 00:13] falls asleep
[1518-11-02 00:36] wakes up
[1518-11-03 00:04] Guard #73 begins shift
[1518-11-03 00:15] falls asleep
[1518-11-03 00:46] wakes up
[1518-11-03 23:57] Guard #3499 begins shift
[1518-11-04 00:38] falls asleep
[1518-11-04 00:55] wakes up
[1518-11-04 23:56] Guard #3541 begins shift
[1518-11-05 00:12] falls asleep
[1518-11-05 00:39] wakes up
[1518-11-05 00:46] falls asleep
[1518-11-05 00:47] wakes up
[1518-11-05 00:55] falls asleep
[1518-11-05 00:59] wakes up
[1518-11-05 23:58] Guard #3499 begins shift
[1518-11-06 00:27] falls asleep
[1518-11-06 00:59] wakes up
[1518-11-06 23:58] Guard #401 begins shift
[1518-11-07 00:26] falls asleep
[1518-11-07 00:48] wakes up
[1518-11-08 00:00] Guard #2861 begins shift
[1518-11-08 00:09] falls asleep
[1518-11-08 00:57] wakes up
[1518-11-08 23:56] Guard #1831 begins shift
[1518-11-09 00:23] falls asleep
[1518-11-09 00:59] wakes up
[1518-11-09 23:59] Guard #73 begins shift
[1518-11-10 00:26] falls asleep
[1518-11-10 00:30] wakes up
[1518-11-10 00:48] falls asleep
[1518-11-10 00:59] wakes up
[1518-11-11 00:00] Guard #1409 begins shift
[1518-11-11 00:39] falls asleep
[1518-11-11 00:54] wakes up
[1518-11-12 00:03] Guard #1877 begins shift
[1518-11-12 00:50] falls asleep
[1518-11-12 00:58] wakes up
[1518-11-12 23:59] Guard #2663 begins shift
[1518-11-13 00:14] falls asleep
[1518-11-13 00:47] wakes up
[1518-11-14 00:04] Guard #2663 begins shift
[1518-11-14 00:15] falls asleep
[1518-11-14 00:42] wakes up
[1518-11-14 00:51] falls asleep
[1518-11-14 00:52] wakes up
[1518-11-15 00:03] Guard #73 begins shift
[1518-11-15 00:11] falls asleep
[1518-11-15 00:26] wakes up
[1518-11-15 00:35] falls asleep
[1518-11-15 00:38] wakes up
[1518-11-15 00:50] falls asleep
[1518-11-15 00:52] wakes up
[1518-11-15 00:56] falls asleep
[1518-11-15 00:59] wakes up
[1518-11-16 00:03] Guard #2861 begins shift
[1518-11-16 00:36] falls asleep
[1518-11-16 00:41] wakes up
[1518-11-16 00:57] falls asleep
[1518-11-16 00:58] wakes up
[1518-11-16 23:48] Guard #2861 begins shift
[1518-11-17 00:00] falls asleep
[1518-11-17 00:49] wakes up
[1518-11-18 00:04] Guard #2411 begins shift
[1518-11-18 00:37] falls asleep
[1518-11-18 00:41] wakes up
[1518-11-18 00:44] falls asleep
[1518-11-18 00:52] wakes up
[1518-11-18 23:57] Guard #1409 begins shift
[1518-11-19 00:16] falls asleep
[1518-11-19 00:27] wakes up
[1518-11-19 00:40] falls asleep
[1518-11-19 00:45] wakes up
[1518-11-19 23:54] Guard #3271 begins shift
[1518-11-20 00:04] falls asleep
[1518-11-20 00:07] wakes up
[1518-11-20 00:44] falls asleep
[1518-11-20 00:52] wakes up
[1518-11-20 23:56] Guard #433 begins shift
[1518-11-21 00:38] falls asleep
[1518-11-21 00:40] wakes up
[1518-11-21 23:56] Guard #433 begins shift
[1518-11-22 00:38] falls asleep
[1518-11-22 00:55] wakes up
[1518-11-23 00:00] Guard #3271 begins shift
[1518-11-23 00:07] falls asleep
[1518-11-23 00:57] wakes up

-}
