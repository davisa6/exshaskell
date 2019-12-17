tes :: String -> Char -> String
tes [] _ = []
tes (x:xs) y | y == x = tes  xs y
             | otherwise = x: tes  xs y

vaz :: String -> Char -> String
vaz [] _ = []
vaz (x:xs) y | y == x = []
             | otherwise = x: vaz xs y

sW :: String -> Char -> [String]
sW [] _ = []
sW x y = (vaz x y) : sW ( tes x y) y
