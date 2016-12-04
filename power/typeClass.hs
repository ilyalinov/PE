data Term = Var String | App Term Term deriving Show

class Language a where
  cond :: a -> a -> a -> a
  
instance Language Term where
  cond a b c = App (Var "cond") (App a (App b c))

instance Language Int where
  cond a b c = if a > b then b else c 

test x y z = cond x y z

main = 
 do
   putStrLn $ show (test (1::Int) (2::Int) (3::Int))
   putStrLn $ show (test (Var "a") (Var "b") (Var "c"))

