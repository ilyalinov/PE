perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = 
    let f y acc []     = (reverse acc ++ [y]) : []
        f y acc (x:xs) = (reverse acc ++ [y] ++ (x:xs)) : (f y (x:acc) xs)
    in concatMap (f x []) l
        where l = perms xs  

