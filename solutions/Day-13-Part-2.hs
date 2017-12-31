import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafePerformIO)

puzzleInput = unsafePerformIO $ readFile "./Day-13-Input.txt"

data Scanner = Scanner { depth :: Int, range :: Int } deriving (Show)

readScanner :: String -> Scanner
readScanner s = let [depth, range] = splitOn ": " s in
                    Scanner { depth = read $ takeWhile ((/=) ':') depth,
                              range = read range }

caught :: Int -> Scanner -> Bool
caught delay Scanner { depth = depth, range = range }
  = (depth + delay) `mod` ((range - 1) * 2) == 0

-- brute force
delay :: [Scanner] -> Int
delay = delay' 0
    where delay' :: Int -> [Scanner] -> Int
          delay' n scanners = if any (caught n) scanners
                                 then delay' (n + 1) scanners
                                 else n

solve :: String -> Int
solve = delay
    . map readScanner
    . lines

solution = solve puzzleInput

main = print solution

