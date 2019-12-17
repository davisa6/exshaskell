group :: Eq t => [[t]] -> [(t, Int)]
group [] = []
group x = aux(juntalista x)

aux :: Eq t => [t] -> [(t, Int)]
aux [] = []
aux x = tuplou (x): aux (removeu (head x) (tail x))

quantas :: Eq t => t -> [t] -> Int
quantas _ [] = 0
quantas y (x:xs) | y == x = 1+(quantas y xs)
                 | otherwise = 0+ quantas y xs

tuplou :: Eq t => [t] -> (t, Int)
tuplou (x:xs) = (x, 1+quantas x xs) 

removeu :: Eq t => t -> [t] -> [t]
removeu _ [] = []
removeu y (x:xs) | y == x = removeu y xs 
                 | otherwise = x: removeu y xs

juntalista :: Eq t => [[t]] -> [t]
juntalista [] = []
juntalista (x:xs) = x ++ juntalista xs