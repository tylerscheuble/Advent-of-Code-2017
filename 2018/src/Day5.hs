module Day5 where

import           Data.Char        (isLetter, isLower, isUpper, toLower, toUpper)
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/Day5.txt"

swapCase :: Type -> Type
swapCase c | isUpper c = toLower c
           | isLower c = toUpper c
           | otherwise = c

type Type = Char
type Polymer = String

reactsTo :: Type -> Type -> Bool
reactsTo a b = a /= b && a == swapCase b

sameType :: Type -> Type -> Bool
sameType a b = a == b || a == swapCase b

react :: Polymer -> Polymer
react = foldr react' ""
  where react' x [] = [x]
        react' x (y:ys)
          | x `reactsTo` y = ys
          | otherwise      = x:y:ys

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
