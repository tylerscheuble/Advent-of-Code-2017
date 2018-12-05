module Day3 where

import           Data.Char        (isDigit)
import           System.IO.Unsafe (unsafePerformIO)

segmentBy :: (a -> Bool) -> [a] -> [[a]]
segmentBy _ [] = []
segmentBy p l = x : segmentBy p rest
  where (x, rest) = span p $ dropWhile (not . p) l

data Claim = Claim { claimId :: Int,
                     xMin    :: Int,
                     yMin    :: Int,
                     xMax    :: Int,
                     yMax    :: Int } deriving (Show)

c1 = parseClaim "#1 @ 1,3: 4x4"
c2 = parseClaim "#2 @ 3,1: 4x4"
c3 = parseClaim "#3 @ 5,5: 2x2"

parseClaim :: String -> Claim
parseClaim s = Claim { claimId = cId,
                       xMin = offX,
                       yMin = offY,
                       xMax = offX + szX,
                       yMax = offY + szY }
  where [ cId, offX, offY, szX, szY ] = map read $ segmentBy isDigit s

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "../input/Day3.txt"

--solve1 :: String -> Int
solve1 = map parseClaim
  . lines

solve2 :: String -> Int
solve2 = read

main = do
  print $ solve1 input
  print $ solve2 input
