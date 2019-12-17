invertelista :: String -> String
invertelista [] = []
invertelista (x:xs) = invertelista xs ++ [x]

listavaz :: String -> Char -> String
listavaz [] _ = []
listavaz (x:xs) y | y == x = []
                  | otherwise = x:listavaz xs y

remove :: String -> Char -> String
remove [] _= []
remove (x:xs) y | y == x = xs
                | otherwise = remove xs y

tiraultimo :: String -> String
tiraultimo [] = []
tiraultimo x = init x

invertpre :: String -> Char -> String
invertpre [] _ = []
invertpre x y = invertelista (listavaz x y) ++ [y] ++ invertpre(listavaz x y)y

invertpos :: String -> Char -> String
invertpos [] _ = []
invertpos x y = tiraultimo (invertpre x y)

invert :: String -> Char -> String
invert [] _ = []
invert x y | last x == y = invertpre x y
           | otherwise = invertpos x y