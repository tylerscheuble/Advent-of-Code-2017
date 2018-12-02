import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)

puzzleInput = unsafePerformIO $ readFile "./Day-4-Input.txt"

anagrams :: String -> String -> Bool
anagrams a b = sort a == sort b

validPhrase :: [String] -> Bool
validPhrase [] = True
validPhrase (x:xs) = if any (anagrams x) xs then False else validPhrase xs

solve :: String -> Int
solve = length . filter validPhrase . map words . lines 

solution = solve puzzleInput

main = print solution

