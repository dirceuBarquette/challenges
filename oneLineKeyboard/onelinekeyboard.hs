import Data.List (elemIndex)

main :: IO ()
main = do
   putStrLn "Insira o teclado"
   kb <- getLine
   putStrLn "Insira a palavra"
   word <- getLine
   putStrLn $ show $ parse kb word

parse :: String -> String -> Int
parse = go (0, 0)
   where
      go (_, acum) _ []        = acum
      go (ini, acum) kb (x:xs) = go (ix, acum') kb xs
         where
            ix = case elemIndex x kb of
                  Just n  -> n
                  Nothing -> 0
            acum' | ix > ini  = ix - ini + acum
                  | otherwise = ini - ix + acum
