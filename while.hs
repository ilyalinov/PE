infix  1 <:=
infixr 0 |>
infixl 6 <+>
infixl 6 <->
infixl 7 <**>
infixl 7 </>
infixl 7 <%>
infix  4 <<=
infix 4 <==>

update st x v    = \ y -> if x == y then v else st y
if_ e s1 s2      = \ st -> if e st == 1 then s1 st else s2 st
while e s        = \ st -> if e st == 1 then while e s (s st) else st
repuntil e s     = \ st -> let s1 = s st 
                           in if e s1 == 1 then s1 else (repuntil e s s1)
skip             = id
x <:= e          = \ st -> update st x (e st)
s1 |> s2         = s2 . s1 

lit x            = \ st -> x
var x            = \ st -> st x
e1 <+> e2        = \ st -> e1 st + e2 st
e1 <-> e2        = \ st -> e1 st - e2 st
e1 <**> e2       = \ st -> e1 st * e2 st
e1 </> e2        = \ st -> e1 st `div` e2 st
e1 <%> e2        = \ st -> e1 st `mod` e2 st
e1 <<= e2        = \ st -> if e1 st <= e2 st then 1 else 0 -- ?
e1 <==> e2       = \ st -> if e1 st == e2 st then 1 else 0 -- ?

fact n = 
  ("f" <:= lit 1 |>   
   while (lit 1 <<= var "n") 
         ("f" <:= var "f" <**> var "n" |>
          "n" <:= var "n" <-> lit 1
         )
  ) st0 $ "f" where
  st0 "n" = n

fact' n = ("r" <:= lit 1 |>
        repuntil (var "n" <<= lit 1)
                 ("r" <:= var "r" <**> var "n" |>
                  "n" <:= var "n" <-> lit 1
                 )
      ) st0 $ "r" where
      st0 "n" = n

add x y = ("r" <:= var "x" <+> var "y") st0 $ "r" 
           where
           st0 "x" = x
           st0 "y" = y


pow a n =
    (
        "r" <:= lit 1 |>
        while (lit 1 <<= var "n")
        (
            if_ (lit 0 <==> var "n" <%> lit 2)
            (
                "n" <:= var "n" </> lit 2 |>
                "a" <:= var "a" <**> var "a"
            )
            (
                "n" <:= var "n" <-> lit 1 |>
                "r" <:= var "r" <**> var "a"
            )
        )
    ) st0 $ "r" where
                st0 "a" = a
                st0 "n" = n

-- repeat .. until +
-- больше арифметики +
-- добавим ввод-вывод: read (x) write (e)
-- написать возведение в степень разложением показателя по степени двойки +
-- подумать о более пристойном внешнем представлении
--   var "f" <**> var "n" 
--   f * n
