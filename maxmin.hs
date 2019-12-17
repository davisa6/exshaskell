maxa :: [(Int, Int)] -> (Int, Int)
maxa [] = (0,0)
maxa (x:xs) | ((fst x > fst (maxa xs)) && (fst x > snd (maxa xs))) || ((snd x > fst (maxa xs)) && (snd x > snd (maxa xs)))   = x
            | otherwise = maxa xs

maxMin :: [(Int, Int)] ->((Int, Int), (Int, Int))
maxMin [] = ((0,0),(0,0))
maxMin x = ((maxa x),(mina x))

mina :: [(Int, Int)] -> (Int, Int)
mina (x:[]) = x
mina (x:xs) | ((fst x < fst (mina xs)) && (fst x < snd (mina xs))) || ((snd x < fst (mina xs)) && (snd x < snd (mina xs)))   = x
            | otherwise = mina xs
