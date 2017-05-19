data V = S Int | D String 

instance Show V where
  show (S n) = show n
  show (D s) = s

data E = Add E E | Sub E E | Const Int | Var String 

eval st (Add x y) = eval' st ((+), "+") x y
eval st (Sub x y) = eval' st ((-), "-") x y
eval st (Const n) = S n
eval st (Var   x) = st x

eval' st (op, op') x y =
  case (eval st x, eval st y) of
    (S x', S y') -> S $ x' `op` y'
    (  x',   y') -> D $ show x' ++ op' ++ show y'
