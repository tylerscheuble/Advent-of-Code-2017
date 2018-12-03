import           Data.Foldable    (elem)
import           Data.List        (find, intersect, tails)
import           Data.Map.Strict  as Map
import           Data.Maybe       (fromJust)
import           System.IO.Unsafe (unsafePerformIO)

input :: String
input = unsafePerformIO $ readFile "./Day2-Input.txt"

type Histogram a = Map a Int

histogram :: Ord a => [a] -> Histogram a
histogram []     = empty
histogram (x:xs) = insertWith (+) x 1 $ histogram xs

countBy :: (a -> Bool) -> [a] -> Int
countBy _ [] = 0
countBy p (x:xs)
  | p(x)      = 1 + countBy p xs
  | otherwise = countBy p xs

checksum :: [String] -> Int
checksum xs = countBy (elem 2) hists * countBy (elem 3) hists
  where hists = fmap histogram xs

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

differences :: String -> String -> Int
differences a b = sum $ zipWith (\x y -> if x /= y then 1 else 0) a b

solve1 :: String -> Int
solve1 = checksum . lines

-- brute forcing is fast enough
solve2 :: String -> String
solve2 = (\(a, b) -> intersect a b)
  . fromJust
  . find (\(a, b) -> differences a b == 1)
  . pairs
  . lines

main :: IO ()
main = do
  print $ solve1 input
  print $ solve2 input
