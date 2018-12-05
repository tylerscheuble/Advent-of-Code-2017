module Day1 where

import           Data.Foldable    (foldMap)
import           Data.Maybe       (fromJust)
import           Data.Monoid
import           Data.Set         as Set
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "./Day1-Input.txt"

-- read can't handle unary plus
readFrequency :: String -> Sum Int
readFrequency ('+':xs) = Sum $ read xs
readFrequency x        = Sum $ read x

repeating :: [a] -> [a]
repeating = concat . repeat

concatenations :: Monoid m => m -> [m] -> [m]
concatenations p []     = [p]
concatenations p (x:xs) = p : concatenations (p <> x) xs

firstDup :: Ord a => [a] -> Maybe a
firstDup = firstDup' empty
  where firstDup' :: Ord a => Set a -> [a] -> Maybe a
        firstDup' _ [] = Nothing
        firstDup' prev (x:xs)
          | member x prev = Just x
          | otherwise = firstDup' (insert x prev) xs

solve1 :: String -> Int
solve1 = getSum
  . foldMap readFrequency
  . lines

solve2 :: String -> Int
solve2 = fromJust
  . firstDup
  . fmap getSum
  . concatenations mempty
  . repeating
  . fmap readFrequency
  . lines

solution1 = solve1 input

solution2 = solve2 input
