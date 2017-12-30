import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafePerformIO)

puzzleInput = head $ lines $ unsafePerformIO $ readFile "./Day-11-Input.txt"

data Translation = Translation Int Float deriving (Show)

instance Monoid Translation where
    mempty = Translation 0 0
    mappend (Translation aX aY) (Translation bX bY) = Translation (aX + bX) (aY + bY)

readTranslation :: String -> Translation
readTranslation d = case d of
                  "n"  -> Translation 0 1.0
                  "ne" -> Translation 1 0.5
                  "se" -> Translation 1 (-0.5)
                  "s"  -> Translation 0 (-1.0)
                  "sw" -> Translation (-1) (-0.5)
                  "nw" -> Translation (-1) 0.5

distance :: Translation -> Int
distance (Translation x y) = absX + ceiling (absY - fromIntegral absX / 2)
    where absX = abs x
          absY = abs y

solve :: String -> Int
solve = distance
    . foldMap readTranslation
    . splitOn ","

solution = solve puzzleInput

main = print solution

