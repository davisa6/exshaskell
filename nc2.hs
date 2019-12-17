primo :: Int -> Bool
primo 0 = False
primo 1 = False
primo x = length[y | y<-[2..x-1],mod x y == 0]==0

listprime :: [Int] -> [Int]
listprime [] = []
listprime x = filter primo x

notprime :: Int -> Bool
notprime 0 = False
notprime 1 = True
notprime x = length[y | y<-[2..x-1],mod x y == 0]/=0

listnotprime :: [Int] -> [Int]
listnotprime [] = []
listnotprime x = filter notprime x