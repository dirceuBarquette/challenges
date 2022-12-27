import Data.List (nub)

main = do
   putStrLn $ show $ parse . combs $ nums

nums = [3,30,34,5,9]

parse :: [[Integer]] -> Integer
parse = foldl (\acc x-> if num x > acc then num x else acc) 0
   where
      num x = toNum . toStr $ x

shiftHead :: Num a => [a] -> [[a]]
shiftHead xs = go [xs] [] xs
   where
      go l _ [] = l
      go l _ (_:[]) = go l [] []
      go l l' (x:(y:ys)) = go ((shiftH++[x]++ys):l) shiftH (x:ys)
         where 
            shiftH = l' ++ [y]

combs :: (Eq a, Num a) => [a] -> [[a]]
combs xs = go (shiftHead xs) xs
   where
      go l [] = l
      go l (_:ys) = go (nub $ foldl (\acc x-> acc++(shiftHead x)) l l) ys

toStr :: (Num a, Show a) => [a] -> [String]
toStr = map show

toNum :: [String] -> Integer
toNum n = read (concat n)::Integer
