import           Data.Foldable    (foldMap)
import           Data.Monoid
import           System.IO.Unsafe (unsafePerformIO)

input :: String
input = unsafePerformIO $readFile "./Day-1-Input.txt"

-- read can't handle unary plus
readFrequency :: String -> Sum Int
readFrequency ('+':xs) = Sum (read xs)
readFrequency x        = Sum (read x)

solve :: String -> Int
solve = getSum . foldMap readFrequency  . lines

main :: IO ()
main = print $ solve input

