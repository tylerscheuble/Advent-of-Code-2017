import System.IO.Unsafe (unsafePerformIO)

puzzleInput = unsafePerformIO $ readFile "./Day-4-Input.txt"

validPhrase :: [String] -> Bool
validPhrase [] = True
validPhrase (x:xs) = if elem x xs then False else validPhrase xs

solve :: String -> Int
solve = length . filter validPhrase . map words . lines 

solution = solve puzzleInput

main = print solution

