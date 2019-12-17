aplica :: (t1,t1->t2) -> t2
aplica (e,f) = f e

aplicafuncoes :: [(t, t->b)] -> [b]
aplicafuncoes a = map (aplica) a