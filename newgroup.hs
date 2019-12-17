group :: [(String, Int)] -> [(String,[Int])]
group [] = []
group (x:xs) = (acumula' (fst x) (x:xs)) : group (remove (fst x )xs)

remove :: String -> [(String, Int)] -> [(String, Int)]
remove y [] = []
remove y (x:xs) | y == fst x = remove y xs
                | otherwise = x: remove y xs

acumula :: String -> [(String, Int)] -> [Int]
acumula y [] = []
acumula y (x:xs) | y == fst x = snd x : acumula y xs
                 | otherwise = acumula y xs

acumula' :: String -> [(String, Int)] -> (String, [Int])
acumula' y x = (y, acumula y x) 



sec :: (String, Int) -> Int 
sec (a,b) = b

auxa x = map sec x

fro :: (String, Int) -> String
fro (a,b) = a


--compara :: [(String, Int)] -> [Int]
--compara [] = []
--compara (x:xs) | fst x == fst (compara xs) = snd x : snd (compara xs) 

