module Day7 where

import           Data.Char        (isUpper)
import           Data.List        (foldl')
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (fromJust, fromMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/Day7.txt"

listToPair :: [a] -> (a, a)
listToPair (a:b:_) = (a, b)
listToPair _       = error "Less than two elements"

findPairWith :: (v -> Bool) -> Map k v -> Maybe (k, v)
findPairWith p m
  | Map.null m = Nothing
  | p v        = Just (k, v)
  | otherwise  = findPairWith p (Map.drop 1 m)
  where (k, v) = Map.elemAt 0 m

-- adjust with a default value
adjust' :: Ord k => (v -> v) -> v -> k -> Map k v -> Map k v
adjust' f d = Map.alter (Just . f . fromMaybe d)

type Step = Char

-- keys are steps, values are the set of steps that must be completed before the key step
type Requirements = Map Step (Set Step)

data Rule = Rule { requirement :: Step, for :: Step } deriving (Show)

rule :: Step -> Step -> Rule
rule a b = Rule { requirement = a, for = b }

parseRule :: String -> Rule
parseRule = uncurry rule
  . listToPair
  . tail
  . filter isUpper

requirements :: [Rule] -> Requirements
requirements rs = foldl' add initial rs
  where initial :: Requirements
        initial = Map.fromList [(x, Set.empty) | x <- concatMap (\x -> [for x, requirement x]) rs]

        add :: Requirements -> Rule -> Requirements
        add m x = Map.adjust (Set.insert $ requirement x) (for x) m

order :: Requirements -> [Step]
order rs
  | Map.null rs = []
  | otherwise   = s : order (Map.map (Set.delete s) $ Map.delete s rs)
  where s = fst $ fromJust $ findPairWith null rs

solve1 :: String -> String
solve1 = order
  . requirements
  . map parseRule
  . lines

solve2 :: String -> Int
solve2 = undefined

main = do
  print $ solve1 input
  print $ solve2 input
