module DSL where

{- 
Module where our language is defined. 
-}

import Prelude hiding (Eq, Ord,  Integral, (==), (/=), (<), (>), (<=), (>=), div, mod)

data T = T ((String -> Int) -> Int)

class Eq a where
    infix 4 ==
    infix 4 /=
    (==) :: a -> a -> a
    (/=) :: a -> a -> a
    
class Ord a where
    infix 4 >
    infix 4 <
    infix 4 >=
    infix 4 <=
    (>) :: a -> a -> a
    (<) :: a -> a -> a
    (>=) :: a -> a -> a
    (<=) :: a -> a -> a
    
class Integral a where
    infixl 7 `div`
    infixl 7 `mod`
    div :: a -> a -> a
    mod :: a -> a -> a

instance Eq Int where
    (==) x y = case compare x y of
                    EQ -> 1
                    _  -> 0
    (/=) x y = 1 - (x == y)
    
instance Eq T where
    (==) (T e1) (T e2) = T (\ st -> e1 st == e2 st)
    (/=) (T e1) (T e2) = T (\ st -> e1 st /= e2 st)
    
instance Ord Int where
    (>) x y = case compare x y of 
                    GT -> 1
                    _  -> 0
    (<) x y = case compare x y of
                    LT -> 1
                    _  -> 0
    (>=) x y = case compare x y of
                    LT -> 0
                    _  -> 1
    (<=) x y = case compare x y of 
                    GT -> 0
                    _  -> 1
    
instance Ord T where
    (<) (T e1) (T e2) = T (\ st -> e1 st < e2 st)
    (>) (T e1) (T e2) = T (\ st -> e1 st > e2 st)
    (<=) (T e1) (T e2) = T (\ st -> e1 st <= e2 st)
    (>=) (T e1) (T e2) = T (\ st -> e1 st >= e2 st)
    
instance Num T where
    (+) (T e1) (T e2) = T (\ st -> e1 st + e2 st)
    (-) (T e1) (T e2) = T (\ st -> e1 st - e2 st)
    (*) (T e1) (T e2) = T (\ st -> e1 st * e2 st)
    abs (T e)         = undefined
    signum (T e)      = undefined
    fromInteger x     = undefined
    
instance Integral Int where
    div x y = quot x y
    mod x y = rem x y
    
instance Integral T where
    div (T e1) (T e2) = T (\ st -> div (e1 st) (e2 st))
    mod (T e1) (T e2) = T (\ st -> mod (e1 st) (e2 st))
    
update st x v        = \ y -> case compare x y of 
                                    EQ -> v
                                    _  -> st y
if_ (T e) s1 s2      = \ st -> case e st of 
                                    1 -> s1 st 
                                    0 -> s2 st
while (T e) s        = \ st -> case e st of 
                                    1 -> while (T e) s (s st)
                                    0 -> st
repuntil (T e) s     = \ st -> let s1 = s st 
                               in case e s1 of 
                                    1 -> s1
                                    0 -> (repuntil (T e) s s1)
skip                 = id

infix  1 <:=
infixr 0 |>

x <:= (T e)          = \ st -> update st x (e st)
s1 |> s2             = s2 . s1 
lit x                = T (\ st -> x)
var x                = T (\ st -> st x)


-- Tests:

fact :: Int -> Int
fact n = 
  ("f" <:= lit 1 |>   
   while (lit 1 <= var "n") 
         ("f" <:= var "f" * var "n" |>
          "n" <:= var "n" - lit 1
         )
  ) st0 $ "f" where
  st0 "n" = n

fact' :: Int -> Int
fact' n = ("r" <:= lit 1 |>
        repuntil (var "n" <= lit 1)
                 ("r" <:= var "r" * var "n" |>
                  "n" <:= var "n" - lit 1
                 )

      ) st0 $ "r" where
      st0 "n" = n

add :: Int -> Int -> Int
add x y = ("r" <:= var "x" + var "y") st0 $ "r" where
           st0 "x" = x
           st0 "y" = y
    
pow :: Int -> Int -> Int
pow n a =
    (
        "r" <:= lit 1 |>
        while (lit 1 <= var "n")
        (
            if_ (lit 0 == var "n" `mod` lit 2)
            (
                "n" <:= var "n" `div` lit 2 |>
                "a" <:= var "a" * var "a"
            )
            (
                "n" <:= var "n" - lit 1 |>
                "r" <:= var "r" * var "a"
            )
        )
    ) st0 $ "r" where
                st0 "a" = a
                st0 "n" = n
 
powGen :: Int -> String 
powGen n = "spec" ++ show n ++ " a = " ++ "(" ++ "\"r\" <:= lit 1" ++ helper n ++ ")" ++ " st0 $ \"r\" where st0 \"a\" = a" where
                helper 0 = ""
                helper n = case (compare (n `mod` 2) 0) of 
                            EQ -> " |> \"a\" <:= var \"a\" * var \"a\"" ++ helper (n `div` 2)
                            _  -> " |> \"r\" <:= var \"r\" * var \"a\"" ++ helper (n - 1)
                            
spec0 a = ("r" <:= lit 1) st0 $ "r" where st0 "a" = a
                            
spec10 a = ("r" <:= lit 1 
            |> "a" <:= var "a" * var "a" 
            |> "r" <:= var "r" * var "a" 
            |> "a" <:= var "a" * var "a" 
            |> "a" <:= var "a" * var "a" 
            |> "r" <:= var "r" * var "a") st0 $ "r" where st0 "a" = a