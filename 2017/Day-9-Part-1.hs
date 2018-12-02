import System.IO.Unsafe (unsafePerformIO)

type Group = [Thing]
data Thing = Group Group | Garbage String deriving (Show)

puzzleInput :: Group
puzzleInput = parse . unsafePerformIO $ readFile "./Day-9-Input.txt"

garbage :: String -> (String, String)
garbage ('>':cs) = ("", cs)
garbage ('!':cs) = garbage $ tail cs
garbage (c:cs)   = let (rest, remainder) = garbage cs in
                       (c:rest, remainder)

parse :: String -> Group
parse = fst . _parse
    where _parse :: String -> (Group, String)
          _parse "" = ([], "")
          _parse ('}':cs) = ([], cs)
          _parse ('{':cs) = let (grp, grpRemainder) = _parse cs
                                (rest, remainder) = _parse grpRemainder in
                                (Group grp:rest, remainder)
          _parse ('<':cs) = let (grb, grbRemainder) = garbage cs
                                (rest, remainder) = _parse grbRemainder in
                                (Garbage grb:rest, remainder)
          _parse (_:cs) = _parse cs

score :: Int -> Thing -> Int
score n (Group g) = n + (sum $ map (score $ n+1) g)
score _ (Garbage _) = 0

solve :: Thing -> Int
solve = score 0

solution = solve $ Group puzzleInput

main = print solution

