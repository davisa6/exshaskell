rota :: Int -> [Int] -> Int
rota _ [] = 0
rota x y | x > length (y) = (x - length(y))
         | otherwise = x
  

remove :: Int -> [(Int, Int)] -> [(Int, Int)]
remove _ [] = []
remove y (x:xs) | y == snd x = remove y xs
                | otherwise = x:remove y xs


--removemai :: Int -> [Int] -> [Int]
--removemai _ [] = []
--removemai y (x:xs) | y == maximum x = remove y xs
--                   | otherwise = x:remove y xs

posicoes :: Int -> [Int] -> [(Int, Int)]
posicoes _ [] = []
posicoes y (x:xs) = (x,y) : posicoes (y+1) xs

nmaior :: Int -> [(Int, Int)] -> Int
nmaior _ [] = 0 
nmaior 1 x = (snd (maximum x))
nmaior y x = nmaior (y-1) (remove(snd (maximum x))x)

tuplista :: [(t, Int)] -> [t]
tuplista [] = []
tuplista (x:xs) = fst x : tuplista xs

remlar :: Int -> [Int] -> [Int]
remlar _ [] = []
remlar x y = tuplista(remove(nmaior x (posicoes 0 y))(posicoes 0 y))

nmenor :: Int -> [(Int, Int)] -> Int
nmenor _ [] = 0 
nmenor 1 x = (snd (minimum x))
nmenor y x = nmenor (y-1) (remove(snd (minimum x))x)