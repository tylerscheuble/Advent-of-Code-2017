module Day5 where

import           Data.Char        (isLetter, isLower, isUpper, toLower, toUpper)
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/Day5.txt"

swapCase :: Char -> Char
swapCase c | isUpper c = toLower c
           | isLower c = toUpper c
           | otherwise = c

type Polymer = String

reactsTo :: Char -> Char -> Bool
reactsTo a b = a /= b && a == swapCase b

sameType :: Char -> Char -> Bool
sameType a b = a == b || a == swapCase b

react :: Polymer -> Polymer
react []  = []
react [x] = [x]
react [a, b]
  | a `reactsTo` b = []
  | otherwise      = [a, b]
react (a:b:xs)
  | a `reactsTo` b = react xs
  | otherwise      = react (a : take 1 rest) ++ drop 1 rest
  where rest       = react (b:xs)

solve1 :: String -> Int
solve1 = length
  . react
  . filter isLetter

solve2 :: String -> Int
solve2 = minimum
  . map (length . react)
  . (\x -> [ filter (not . sameType t) x | t <- ['A'..'Z'] ])
  . react
  . filter isLetter

main = do
  print $ solve1 input
  print $ solve2 input
