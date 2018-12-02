import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

puzzleInput = map parseInstruction . lines . unsafePerformIO $ readFile "./Day-8-Input.txt"

type Register = String
type Registers = Map Register Int

data Instruction = Instruction { register :: Register,
                                 function :: (Int -> Int),
                                 predicateRegister :: Register,
                                 predicate :: (Int -> Bool) }

parseInt :: String -> Int
parseInt = read

parseInstruction :: String -> Instruction
parseInstruction s = let [r, f, n, _, cr, cc, cn] = words s in
                         Instruction { register = r,
                                       function = case f of
                                                    "inc" -> ((+) $ parseInt n)
                                                    "dec" -> (flip (-) $ parseInt n),
                                       predicateRegister = cr,
                                       predicate = case cc of
                                                     ">"  -> (flip (>) $ parseInt cn)
                                                     "<"  -> (flip (<) $ parseInt cn)
                                                     ">=" -> (flip (>=) $ parseInt cn)
                                                     "<=" -> (flip (<=) $ parseInt cn)
                                                     "==" -> ((==) $ parseInt cn)
                                                     "!=" -> ((/=) $ parseInt cn) }

get :: Register -> Registers -> Int
get r = maybe 0 id . Map.lookup r

apply :: Instruction -> Registers -> Registers
apply instruction regs = let reg     = register instruction
                             func    = function instruction
                             predReg = predicateRegister instruction
                             pred    = predicate instruction in
    if pred (get predReg regs)
       then Map.alter (\x -> Just $ func $ maybe 0 id x) reg regs
       else regs

largest :: Int -> Registers -> [Instruction] -> Int
largest n regs [] = n
largest n regs (i:is) = let newRegs = apply i regs in
                            largest (max n $ get (register i) newRegs) newRegs is

solve :: [Instruction] -> Int
solve = largest 0 Map.empty

solution = solve puzzleInput

main = print solution

