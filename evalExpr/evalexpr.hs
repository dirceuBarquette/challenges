{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char (isDigit)

main :: IO ()
main = do
   putStrLn "Insira a expressão"
   ln <- getLine
   putStrLn $ show . head . eval . str2RPN $ ln

data Op = Mns | Add | Div | Tms  
            deriving Show

data Expression = Expr Op Int Int
                     deriving Show

exec :: Op -> Int -> Int -> Int
exec Add x y = x + y
exec Mns x y = x - y
exec Tms x y = x * y
exec Div x y = x `div` y

instance Eq Op where
   Mns == Add = True 
   Add == Mns = True 
   Div == Tms = True
   Tms == Div = True 
   _   == _   = False

instance Ord Op where
   Mns <= Div = True
   Mns <= Tms = True
   Add <= Div = True
   Add <= Tms = True
   _   <= _   = False

readOp :: String -> Op
readOp c | c == "+" = Add
         | c == "-" = Mns
         | c == "/" = Div
         | c == "*" = Tms
         
type OpStack = [String]
type Output  = [String]

str2RPN :: String -> Output
str2RPN = go [] [] 
   where
      go :: OpStack -> Output -> String -> Output
      go ops output [] = output++ops
      go ops output input
         | isNotOp inputHead = go ops (output++[takeNums]) dropNums
         | inputHead == '('  = go ([inputHead]:ops) output decrInput
         | inputHead == ')'  = go (drop 1 $ dropWhile ("("/=) ops) (output++takeWhile ("("/=) ops) decrInput
         | null ops          = go ([inputHead]:ops) output decrInput
         | head ops == "("   = go ([inputHead]:ops) output decrInput
         | inputHeadAsOp > readOp (head ops) =
            go ([inputHead]:ops) output decrInput 
         | inputHeadAsOp <= readOp (head ops) = 
            go (drop 1 ops) (output++[head ops]) input
         | inputHeadAsOp == readOp (head ops) = 
            go (drop 1 ops) (output++[head ops]) input
         where
            takeNums      = takeWhile isNotOp input
            dropNums      = dropWhile isNotOp input
            decrInput     = drop 1 input
            inputHead     = head input
            inputHeadAsOp = readOp [inputHead]

ops :: [Char]
ops = ['(',')','+','-','*','/']

isNotOp :: Char -> Bool
isNotOp c = notElem c $ ops

eval :: [String] -> [Int]
eval = foldl (\acc x-> let (newNumStack,toExec) = breakNumStack acc
                       in
                          if isNum x 
                           then acc++[read x::Int]
                           else newNumStack++[exec (readOp x) (head toExec) (toExec !! 1)]
             ) []

breakNumStack :: [Int] -> ([Int],[Int])
breakNumStack nums = (newNumStack,toExec)
   where
      newNumStack = reverse . drop 2 . reverse $ nums
      toExec      = drop (length nums - 2) nums

isNum :: String -> Bool
isNum = foldl (\acc x-> acc && isDigit x) True
