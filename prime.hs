primo :: Int -> Bool
primo 0 = False
primo 1 = False
primo 2 = False
primo x = length [y | y<-[2..x-1], mod x y == 0] == 0

listprime :: [Int] -> [Int]
listprime x = filter primo x