module Day5 where

import           Data.Char        (isLetter, isLower, isUpper, toLower, toUpper)
import           Data.List        (dropWhileEnd, notElem, nub)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/Day5.txt"

deleteAll :: Ord a => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll e (x:xs) | e == x    = deleteAll e xs
                   | otherwise = x : deleteAll e xs

swapCase :: Char -> Char
swapCase c | isUpper c = toLower c
           | isLower c = toUpper c
           | otherwise = c

reactsTo :: Char -> Char -> Bool
reactsTo a b = a /= b && a == swapCase b

type Polymer = String

react :: Polymer -> Polymer
react [] = []
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
solve2 = undefined

main = do
  print $ solve1 input
  print $ solve2 input
