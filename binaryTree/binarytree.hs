import Data.Char (isDigit)
import GHC.Float (float2Int)

main :: IO ()
main = do 
   putStrLn "Digite a raiz"
   ln <- getLine
   putStrLn "A soma é:"
   putStrLn $ show $ sumDeeperLevel . readInput $ ln

readInput :: String -> [Maybe Int]
readInput = go []
   where
      go c [] = c
      go c s@(x:xs)
         | x == '['  = go c (init xs)
         | isDigit x = go (c++[Just $ toNum s]) $ dropHead s
         | otherwise = go (c++[Nothing]) $ dropHead s

toNum :: String -> Int
toNum xs= read (takeWhile (/=',') xs)::Int

dropHead :: String -> String
dropHead = drop 1 . dropWhile (/=',') 

sumDeeperLevel :: [Maybe Int] -> Int
sumDeeperLevel = justInt . foldl (\acc x-> fmap (+) x <*> acc) (Just 0)
                  . filter (/= Nothing ) . last . getLevels 

getLevels :: [Maybe Int] -> [[Maybe Int]]
getLevels = go (0,[])
   where
      go (_,lvls) [] = lvls 
      go (p,lvls) xs = let pp = float2Int (2**p)
                       in go (p+1,lvls++[take pp xs]) $ drop pp xs

justInt :: Maybe Int -> Int
justInt Nothing  = 0
justInt (Just x) = x
