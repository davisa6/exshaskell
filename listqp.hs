quadrado :: Int -> Int
quadrado 0 = 0 
quadrado x = x*x

qp :: Int -> Int -> [Int]
qp x y | quadrado x <= y = quadrado x: qp(x-1) y
       | otherwise = []

listqp :: Int -> [Int]
listqp 0 = [0]
listqp x = qp 0 x