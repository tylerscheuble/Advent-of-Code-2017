import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

puzzleInput = unsafePerformIO $ readFile "./Day-16-Input.txt"

data Move = Spin Int | Exchange Int Int | Partner Char Char
    deriving (Show)

type Line = [Char]

readMove :: String -> Move
readMove ('s':xs)         = Spin $ read xs
readMove ('x':xs)         = let [a, b] = splitOn "/" xs in
                                Exchange (read a) (read b)
readMove ['p', a, '/', b] = Partner a b

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = let i = length xs - n
               in drop i xs ++ take i xs

switch :: (Int, Int) -> [a] -> [a]
switch (a, b) xs = let a' = min a b
                       b' = max a b
                       elemA = xs !! a'
                       elemB = xs !! b'
                       beginning = take a' xs
                       middle = take (b' - a' - 1) (drop (a' + 1) xs)
                       end = drop (b' + 1) xs
                    in beginning ++ elemB:middle ++ elemA:end

applyMove :: Line -> Move -> Line
applyMove l (Spin n)       = rotate n l
applyMove l (Exchange a b) = switch (a, b) l
applyMove l (Partner a b)  = switch (fromJust $ elemIndex a l, fromJust $ elemIndex b l) l

solve :: String -> String
solve = foldl applyMove ['a'..'p'] . map readMove . splitOn ","

solution = solve puzzleInput

main = print solution

