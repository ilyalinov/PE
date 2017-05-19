integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let 
    helper s 1000 f a b = s
    helper s i f a b = helper (s + (f (p1 i) + f (p2 i)) * h / 2) (i + 1) f a b
    in helper 0 0 f a b
    where
        p1 i = a + i * h
        p2 i = a + (i + 1) * h
        h = (b - a) / 1000
