somalista :: [Int] -> Int
somalista [] = 0 
somalista (x:xs) = x + somalista xs

cond :: Int -> [Int]-> Bool
cond _ [] = False
cond y (x:xs) | x <= somalista (x:xs)= True
              | otherwise = False 

filtra :: Int -> [[Int]] -> [[Int]]
filtra _ [] = [] 
filtra y x = filter (cond y) x
