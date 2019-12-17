bs :: Int -> Int
bs 0 = 0
bs x = mod x 10 + bs (div x 10)

bigSum :: [Int] -> Int
bigSum [] = 0
bigSum (x:xs) | bs x > bs (bigSum xs) = x
              | otherwise = bigSum xs 