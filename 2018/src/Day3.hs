module Day3 where

import           Data.Char        (isDigit)
import           Data.List        (foldl', maximum)
import           Data.Matrix
import           Numeric.Natural
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/Day3.txt"

segmentBy :: (a -> Bool) -> [a] -> [[a]]
segmentBy _ [] = []
segmentBy p l = x : segmentBy p rest
  where (x, rest) = span p $ dropWhile (not . p) l

inc :: Num a => a -> a
inc = (+) 1

update :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
update f p m = setElem (f $ getElem x y m) p m
  where (x, y) = p

data Claim = Claim { claimId :: Int,
                     xMin    :: Int,
                     yMin    :: Int,
                     xMax    :: Int,
                     yMax    :: Int } deriving (Show)

parseClaim :: String -> Claim
parseClaim s = Claim { claimId = cId,
                       xMin = offX,
                       yMin = offY,
                       xMax = offX + szX,
                       yMax = offY + szY }
  where [ cId, offX, offY, szX, szY ] = map read $ segmentBy isDigit s

type Fabric = Matrix Natural

--layClaims :: [Claim] -> Fabric
layClaims claims = foldl' lay fabric claims
  where fabric = zero l l
        l = maximum $ map (\c -> max (xMax c) (yMax c)) claims
        lay :: Fabric -> Claim -> Fabric
        lay f c = foldl' (flip $ update inc) f pnts
          where pnts = [ (x, y) | x <- [xMin c..xMax c], y <- [yMin c..yMax c] ]

solve1 :: String -> Natural
solve1 = sum
  . fmap (min 1)
  . layClaims
  . map parseClaim
  . lines

solve2 :: String -> Int
solve2 = read

main = do
  print $ solve1 input
  print $ solve2 input
