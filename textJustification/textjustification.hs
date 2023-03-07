main :: IO ()
main = do
   putStrLn "Digite o texto a ser justificado!"
   txt <- getLine
   putStrLn "Digite a quantidade de caracteres por linha"
   maxWidth <- getLine
   let mw   = read maxWidth :: Int 
   let gw   = getWords mw $ words txt
   putStrLn $ unwords $ justify mw gw

justify :: Int -> [[String]] -> [String]
justify mw lns = go (length lns) [] lns
   where
      go 0 ws [] = ws
      go n ws (x:xs)
         | n == 1    = go 0 (ws++last) []
         | otherwise = go (n-1) (ws++[concat (insertSpaces (ts,gaps) x)]) xs
         where
            ts = mw - (length . concat $ x)
            gaps = length x - 1
            pad = (concat $ take (mw-(length $ unwords x)) $ repeat " ")
            last = [(unwords x)++pad]

insertSpaces :: (Int,Int) -> [String] -> [String]
insertSpaces (sp,gaps) = go sp 0
   where
      go 0 _ ws = ws
      go n g ws | g < gaps-1  = go (n-1) (g+1) ws'
                | otherwise   = go (n-1) 0 ws'
                where 
                  (_,ws') = foldl (\(i,new) w-> if i==g
                                                   then (i+1,new++[w++" "])
                                                   else (i+1,new++[w])
                                    ) (0,[]) ws

getWords :: Int -> [String] -> [[String]]
getWords mw = go ([],[])
   where
      go ([],ws) []          = ws
      go (ln,ws) []          = go ([],ws++[ln]) []
      go (ln,ws) (x:xs) 
         | evalLn plusX < mw = go (ln++[x],ws) xs
         | evalLn plusX > mw = go ([x],ws++[ln]) xs
         | otherwise         = go ([],ws++[ln++[x]]) xs
         where
            evalLn = length . unwords
            plusX  = ln ++ [x]
