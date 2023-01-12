import Data.Char (isDigit)

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
      go (p,lvls) xs = let chunk | length lvls == 0 = 1
                                 | otherwise = (2*) . length . filter (/=Nothing) $ lvls !! (p-1)
                       in go (p+1,lvls++[take chunk xs]) $ drop chunk xs

justInt :: Maybe Int -> Int
justInt Nothing  = 0
justInt (Just x) = x
