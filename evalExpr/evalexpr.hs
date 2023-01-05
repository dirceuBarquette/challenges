data Op = Mns | Add | Div | Tms  
            deriving (Show,Eq,Ord,Enum,Bounded)

data Expression = Single Int | Expr Op Expression Expression
                     deriving Show

readOp :: String -> Op
readOp c | c == "+" = Add
         | c == "-" = Mns
         | c == "/" = Div
         | c == "*" = Tms

exec :: Op -> Int -> Int -> Int
exec Add x y = x + y
exec Mns x y = x - y
exec Tms x y = x * y
exec Div x y = x `div` y

eval :: Expression -> [Int]
eval (Single x)   = [x]
eval (Expr o l r) = [exec o l' r' | l'<- eval l, r'<- eval r ]

string1 = "1+1"
string2 = "6-4/2"
string3 = "2*(5+5*2)/3+(6/2+8)"

str2Expr :: String -> Expression
str2Expr = go . sepStr
   where
      go (o,l,r)  
         | not . hasOp $ r = Expr (readOp o) (Single (read l::Int)) (Single (read r::Int))
         | otherwise       = Expr (readOp o) (Single (read l::Int)) (go . sepStr $ r)

hasOp :: String -> Bool
hasOp xs | length (filter (\x-> elem x ops) xs) > 0 = True
         | otherwise                                = False

sepStr :: String -> (String,String,String)
sepStr xs = (op,left,right)
  where
    left   = takeWhile isNotOp xs
    right  = drop 1 wholeR
    op     = take 1 wholeR
    wholeR = dropWhile isNotOp xs

ops :: [Char]
ops = ['(',')','+','-','*','/']

isNotOp :: Char -> Bool
isNotOp c = not . elem c $ ops
