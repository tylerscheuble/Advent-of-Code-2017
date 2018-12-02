import System.IO.Unsafe (unsafePerformIO)
import Data.IntMap.Strict (IntMap, empty, fromList, adjust, (!))

puzzleInput = unsafePerformIO $ readFile "./Day-5-Input.txt"

parseInt :: String -> Int
parseInt = read

listToIntMap :: [a] -> IntMap a
listToIntMap l = fromList $ zip [0..length l - 1] l

increment :: Int -> Int
increment = (+) 1

offset :: Int -> Int
offset x = if x >= 3 then x - 1 else x + 1

movesToEnd :: Int -> Int -> IntMap Int -> Int
movesToEnd moves pos instructions =
    if pos < 0 || pos >= length instructions then moves else
    movesToEnd
        (increment moves)
        (pos + (instructions ! pos))
        (adjust offset pos instructions)

solve :: String -> Int
solve = movesToEnd 0 0
    . listToIntMap
    . map parseInt
    . lines

solution = solve puzzleInput

main = print solution

