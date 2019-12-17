primo :: Int -> Bool
primo x | not (True == [mod y x == 0 | y <- [2..x-1]]) = True
        | otherwise = False

listprime :: [Int] -> [Int]
listprime [] = []
listprime x = filter(primo) x