module Day# where

import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/Day#.txt"


solve1 :: String -> Int
solve1 = undefined

solve2 :: String -> Int
solve2 = undefined

main = do
  print $ solve1 input
  print $ solve2 input
