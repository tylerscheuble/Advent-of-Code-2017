import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import System.IO.Unsafe (unsafePerformIO)

puzzleInput = unsafePerformIO $ readFile "./Day-12-Input.txt"

type Program = (Int, Int, [Int])

connection :: String -> Int
connection s = read [ c | c <- s, c /= ',' ]

program :: String -> Program
program s = let ws = words s
                id = read $ head ws
                connections = map connection $ drop 2 ws in
                (id, id, connections)

solve :: String -> Int
solve = length
    . flip reachable 0
    . (\(x, _, _) -> x)
    . graphFromEdges . map program . lines

solution = solve puzzleInput

main = print solution

