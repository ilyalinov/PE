s1 :: Integer -> (Integer, Integer)
s1 x | x < 0 = helper 0 0 (-x)
     | x > 0 = helper 0 0 x
     | x == 0 = (0, 1)
    where
        helper s q 0 = (s, q)
        helper s q x = helper (s + rem x 10) (q + 1) (quot x 10)
