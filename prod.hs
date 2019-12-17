par :: Int -> Bool
par 0 = False
par x | mod x 2 == 0 = True
      | otherwise = False

comb :: [Int] -> [Int] -> [(Int, Int)]
comb _ [] = []
comb y x = [(a,b) | a <- y, b <- x]

combinations :: [Int] -> [Int] -> [(Int, Int)]
combinations _ [] = []
combinations [] _ = []
combinations x y = [(a,b) | a<-x,b<-y]

tuplista :: [(Int, Int)] -> [Int]
tuplista [] = [] 
tuplista (x:xs) = (fst x) : (snd x) : tuplista xs

slp :: [Int] -> Int
slp [] = 0
slp (x:xs) | mod x 2 == 0 = x + slp xs
           | otherwise = slp xs

prod :: [Int] -> [Int] -> ([(Int, Int)],Int)
prod [] _ = ([],0)
prod _ [] = ([],0)
prod x y = (combinations x y, slp(tuplista(combinations x y)))