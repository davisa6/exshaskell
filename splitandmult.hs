splitandmult :: [Int] ->  ([Int],[Int])
splitandmult (x:xs) = (listapar (x:xs), listaimpar (x:xs))
 --pares :: [Int] -> [Int]

par :: Int -> Bool
par 0 = False
par x | mod x 2 == 0 = True
      | otherwise = False

impar :: Int -> Bool
impar 0 = False
impar x | mod x 2 /= 0 = True
        | otherwise = False

listapar :: [Int] -> [Int]
listapar [] = []
listapar x = filter (par )x

listaimpar :: [Int] -> [Int]
listaimpar [] = []
listaimpar x = filter (impar )x