import Data.List (elemIndex)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

puzzleInput = map parseInt $ words $ unsafePerformIO $ readFile "./Day-6-Input.txt"

distance :: Int -> Int -> Int -> Int
distance max a b
  | b > a = b - a
  | otherwise = b + (max - a)

cycles :: [Int] -> [[Int]]
cycles = iterate nextCycle

nextCycle :: [Int] -> [Int]
nextCycle xs = let largest = maximum xs
                   index = fromJust $ elemIndex largest xs
                   val = xs !! index
                   size = length xs
                   additions = val `div` size
                   remainder = val `mod` size in
                   zipWith (+)
                   [ additions
                     - (if i == index then val else 0)
                     + (if distance size index i <= remainder then 1 else 0)
                       | i <- [0..size-1]] xs

takeWhileUnique :: Eq a => [a] -> [a]
takeWhileUnique = reverse . _takeWhileUnique []
    where _takeWhileUnique :: Eq a => [a] -> [a] -> [a]
          _takeWhileUnique prev [] = prev
          _takeWhileUnique prev (x:xs)
            | elem x prev = prev
            | otherwise = _takeWhileUnique (x:prev) xs

parseInt :: String -> Int
parseInt = read

solve :: [Int] -> Int
solve = (\cs -> length cs - (fromJust $ elemIndex (nextCycle $ last cs) cs))
    . takeWhileUnique 
    . cycles

solution = solve puzzleInput

main = print solution

