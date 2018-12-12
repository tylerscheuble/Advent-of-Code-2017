module Day3 where

import           Data.Char        (isDigit)
import           Data.List        (sort)
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/Day3.txt"

segmentBy :: (a -> Bool) -> [a] -> [[a]]
segmentBy _ [] = []
segmentBy p l = x : segmentBy p rest
  where (x, rest) = span p $ dropWhile (not . p) l

data Rectangle = Rectangle { xMin :: Int,
                             yMin :: Int,
                             xMax :: Int,
                             yMax :: Int } deriving (Show)

parseRectangle :: String -> Rectangle
parseRectangle s = Rectangle { xMin = offX,
                               yMin = offY,
                               xMax = offX + szX,
                               yMax = offY + szY }
  where [ cId, offX, offY, szX, szY ] = map read $ segmentBy isDigit s

sweep :: [Rectangle] -> Int
sweep rects = sweep' queue where
  sweep' :: [Int] -> Int
  sweep' [] = 0

  queue :: [Int]
  queue = sort $ concatMap (\r -> [ xMin r, xMax r ]) rects

solve1 :: String -> Int
solve1 = read

solve2 :: String -> Int
solve2 = read

main = do
  print $ solve1 input
  print $ solve2 input
