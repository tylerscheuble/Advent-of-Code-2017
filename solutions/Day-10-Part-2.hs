import Data.Char (ord)
import Data.Bits (xor)
import Numeric (showHex)

puzzleInput :: String
puzzleInput = "230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167"

third :: (a, b, c) -> c
third (_, _, x) = x

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

buckets :: Int -> [a] -> [[a]]
buckets size xs
  | length xs > size = let (elems, rest) = splitAt size xs in
                           elems : (buckets size rest)
  | otherwise = [xs]

leftPad :: Int -> a -> [a] -> [a]
leftPad n x xs = replicate (n - length xs) x ++ xs

reverseN :: Int -> [a] -> [a]
reverseN n xs = let (elems, rest) = splitAt n xs in
                    reverse elems ++ rest

knot :: Int -> Int -> [Int] -> [Int] -> (Int, Int, [Int])
knot pos skip elems [] = (pos, skip, elems)
knot pos skip elems (l:ls)
  = let size = length elems in
        knot ((pos + l + skip) `mod` size)
        (skip + 1)
        (take size $ rotate (size - pos) $ reverseN l $ rotate pos elems)
        ls

sparseHash :: [Int] -> [Int]
sparseHash lengths = third $ knots !! 64
    where knots = iterate (\(pos, skip, elems) -> knot pos skip elems lengths) (0, 0, [0..255])

denseHash :: [Int] -> [Int]
denseHash = map (foldl1 xor) . buckets 16

knotHash :: [Int] -> String
knotHash = foldMap $ leftPad 2 '0' . (\n -> showHex n "")

solve :: String -> String
solve = knotHash
    . denseHash
    . sparseHash
    . flip mappend [17, 31, 73, 47, 23]
    . map ord

solution = solve puzzleInput

main = print solution

