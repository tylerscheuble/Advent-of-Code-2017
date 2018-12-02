import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafePerformIO)

puzzleInput = unsafePerformIO $ readFile "./Day-13-Input.txt"

data Scanner = Scanner { depth :: Int, range :: Int } deriving (Show)

readScanner :: String -> Scanner
readScanner s = let [depth, range] = splitOn ": " s in
                    Scanner { depth = read $ takeWhile ((/=) ':') depth,
                              range = read range }

severity :: Scanner -> Int
severity Scanner { depth=depth, range=range }
  | depth `mod` ((range - 1) * 2) == 0 = depth * range
  | otherwise = 0

solve :: String -> Int
solve = sum
    . map (severity . readScanner)
    . lines

solution = solve puzzleInput

main = print solution

