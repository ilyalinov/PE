data Power = Deg0 | Mult Power | Square Power
data Exp = Exp Int Power

eval :: Exp -> Int
eval (Exp x Deg0) = 1
eval (Exp x (Mult power)) = (eval (Exp x power)) * x
eval (Exp x (Square power)) = (eval (Exp x power)) * (eval (Exp x power))

--3 ^ 0
test0 :: Exp
test0 = Exp 3 Deg0

-- (3 ^ 2) * 3 = 3 ^ 3
test1 :: Exp
test1 = Exp 3 (Mult (Square (Mult Deg0)))

-- (3 ^ 2) ^ 2 = 3 ^ 4
test2 :: Exp
test2 = Exp 3 (Square (Square (Mult Deg0)))

main = do
    putStrLn $ show (eval test0)
    putStrLn $ show (eval test1)
    putStrLn $ show (eval test2)
