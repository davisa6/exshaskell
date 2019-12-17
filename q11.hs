--Defina uma função para, dada uma lista de listas de inteiros, remover da lista todas
--as listas cujas somas dos elementos são menores que um certo valor, também fornecido
--como argumento. Use filter e foldr para resolver esta questão.

somalista :: [Int] -> Int
somalista [] = 0
somalista (x:xs) = x + somalista xs

apai :: Int -> [[Int]] -> [[Int]]
apai _ [] = []
apai y (x:xs)| y>somalista (x:xs) = filter (abu) (x:xs)
             | otherwise = []  

abu :: [Int] -> Int -> Bool
abu [] _= False
abu _ 0 = False
abu (x:xs) y | y > somalista (x:xs) = True
             | otherwise = False
