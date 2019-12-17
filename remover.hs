remover :: [Int] -> [Int]
remover [] = []
remover (x:xs) | elem x xs = remover xs
			   | otherwise = x:remover xs

juntalista :: [[Int]] -> [Int]
juntalista [] = [] 
juntalista (x:xs) = x ++ juntalista xs

remove :: [[Int]] -> [Int]
remove [] = []
remove x = remover(juntalista x) 