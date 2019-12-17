group :: [(String, Int)] -> [(String, [Int])]
group [] = []
group (x:xs) = (tupla (fst x) (x:xs)) : group(remove (fst x) xs)

remove :: String -> [(String, Int)] -> [(String, Int)]
remove _ [] = []
remove y (x:xs) | y == fst x = remove y xs
                | otherwise = x: remove y xs

acumula :: String -> [(String, Int)] -> [Int]
acumula _ [] = []
acumula y (x:xs) | y == fst x = snd x : acumula y xs
                 | otherwise = acumula y xs

tupla :: String -> [(String, Int)] -> (String, [Int])
tupla y (x:xs) | y == fst x = (y, acumula y (x:xs))
               | otherwise = tupla y xs
