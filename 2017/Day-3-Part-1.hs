puzzleInput = 265149

data Ring = Ring { n :: Int,
                   width :: Int,
                   circumference :: Int } deriving (Show)

ring :: Int -> Ring
ring n = let width = n * 2 - 1 in
             Ring { n=n, width = width, circumference = n * 8 + 1 }

area :: Ring -> Int
area r = (width r) ^ 2

difference :: Int -> Int -> Int
difference a b = abs $ b - a

findRing :: Int -> Ring
findRing x = _findRing 0
    where _findRing :: Int -> Ring
          _findRing n = let r = ring n in
                            if x < area r then r else _findRing $ n + 1

distance :: Int -> Int
distance x = (width r) - 1 - (minimum $ map (difference x) corners)
    where corners :: [Int]
          r = findRing x
          highest = area r
          step = (circumference r) `div` 4
          corners = [highest, highest - step, highest - step * 2, highest - step * 3]

solve :: Int -> Int
solve = distance

solution = solve puzzleInput

main = print solution

