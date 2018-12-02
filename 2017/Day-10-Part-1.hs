puzzleInput :: [Int]
puzzleInput = [230, 1, 2, 221, 97, 252, 168, 169, 57, 99, 0, 254, 181, 255, 235, 167]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

reverseN :: Int -> [a] -> [a]
reverseN n xs = let (elems, rest) = splitAt n xs in
                    reverse elems ++ rest

knot :: Int -> Int -> [Int] -> [Int] -> [Int]
knot _ _ elems [] = elems
knot pos skip elems (l:ls)
  = let size = length elems in
        knot ((pos + l + skip) `mod` size)
        (skip + 1)
        (take size $ rotate (size - pos) $ reverseN l $ rotate pos elems)
        ls

solve :: [Int] -> Int
solve = product . take 2 . knot 0 0 [0..255]

solution = solve puzzleInput

main = print solution

