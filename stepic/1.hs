seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n >= 2 = let
           helper acc0 acc1 acc2 2 = acc2
           helper acc0 acc1 acc2 n = helper acc1 acc2 (acc2 + acc1 - 2 * acc0) (n - 1)
           in helper 1 2 3 n
       | otherwise = error "args must be >= 0"
