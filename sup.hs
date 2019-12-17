--retornaSup :: Int -> [Int] -> Int

conta :: Int -> [Int] -> Int
conta _ [] = 0
conta y (x:xs) | y > x = 1+ conta y xs
               | otherwise = conta y xs


retornalistasup :: Int -> [Int] -> [Int]
retornalistasup _ [] = []
retornalistasup y (x:xs) | x > y = x: retornalistasup y xs
                         | otherwise = retornalistasup y xs

uniao :: [Int] -> [Int] -> [Int]
uniao [] _ = []
uniao _ [] = []
uniao (x:xs) (y:ys) | x == y = uniao xs ys
                    | otherwise = x: y: uniao xs ys
            