module Day4 where

import           Data.Char           (isDigit)
import           Data.DateTime       (DateTime, diffMinutes', parseDateTime,
                                      toSeconds)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IntMap
import           Data.List           (break, foldl', mapAccumL, maximumBy)
import           Data.Map.Strict     (Map)
import           Data.Maybe          (fromJust, fromMaybe)
import           Data.Time.Clock     (utctDayTime)
import           Data.Time.LocalTime (TimeOfDay, timeToTimeOfDay, todHour,
                                      todMin)
import           GHC.Exts            (sortWith)
import           System.IO.Unsafe    (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/Day4.txt"

maximumWith :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maximumWith f = maximumBy (\a b -> compare (f a) (f b))

maxPairBy :: Ord b => (a -> b) -> IntMap a -> (IntMap.Key, a)
maxPairBy f = maximumWith (f . snd) . IntMap.toList

maxPair :: Ord a => IntMap a -> (IntMap.Key, a)
maxPair = maxPairBy id

timeOfDay :: DateTime -> TimeOfDay
timeOfDay = timeToTimeOfDay . utctDayTime

parseTime :: String -> DateTime
parseTime = fromJust . parseDateTime "%0Y-%m-%d %H:%M"

data Record = BeginShift Int DateTime | Awake DateTime | Asleep DateTime
  deriving (Show)

parseRecord :: String -> Record
parseRecord s = case msg of
                  "wakes up"     -> Awake time
                  "falls asleep" -> Asleep time
                  _              -> BeginShift (read $ filter isDigit msg) time
  where (time, msg) = let (timeStr, rest) = span (']' /=) $ tail s in
                          (parseTime timeStr, drop 2 rest)

time :: Record -> DateTime
time (Awake t)        = t
time (Asleep t)       = t
time (BeginShift _ t) = t

newShift :: Record -> Bool
newShift (BeginShift _ _) = True
newShift _                = False

minutesOfDay :: TimeOfDay -> Int
minutesOfDay t = todMin t + (todHour t * 60)

data Day = Day { guard       :: Int,
                 asleepTimes :: [(Int, Int)] } deriving (Show)

days :: [Record] -> [Day]
days [] = []
days (BeginShift g t : xs) = day records : days rest
  where (records, rest) = break newShift xs
        day recs = Day { guard = g, asleepTimes = asleep recs }
        asleep :: [Record] -> [(Int, Int)]
        asleep []                        = []
        asleep (Asleep a : Awake b : rs) = (minutesOfDay $ timeOfDay a, minutesOfDay $ timeOfDay b) : asleep rs

-- adjust with a default value
adjust' :: (a -> a) -> a -> IntMap.Key -> IntMap a -> IntMap a
adjust' f d = IntMap.alter (Just . f . fromMaybe d)

type DayMap = IntMap Int

type SleepMap = IntMap DayMap

sleepMap :: [Day] -> SleepMap
sleepMap = foldl' aggregate IntMap.empty
  where aggregate :: SleepMap -> Day -> SleepMap
        aggregate m d = adjust' (IntMap.unionWith (+) dayMap) IntMap.empty (guard d) m
          where dayMap :: DayMap
                dayMap = IntMap.fromList $ concatMap (\(a, b) -> [(x, 1) | x <- [a..b]]) (asleepTimes d)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

solve1 :: String -> Int
solve1 = uncurry (*)
  . mapSnd (\n -> n - 1)
  . mapSnd (fst . maxPair)
  . maxPairBy sum
  . sleepMap
  . days
  . sortWith time
  . map parseRecord
  . lines

solve2 :: String -> Int
solve2 = uncurry (*)
  . mapSnd (\n -> n - 1)
  . mapSnd (fst . maxPair)
  . maxPair
  . sleepMap
  . days
  . sortWith time
  . map parseRecord
  . lines

main = do
  print $ solve1 input
  print $ solve2 input
