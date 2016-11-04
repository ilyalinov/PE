square :: Int -> Int
square x = x * x

f :: Int -> Int -> Int
f n x = 
    if n == 0 then
        1
    else if even n then
        square (f (quot n 2) x)
    else
        (f (n - 1) x) * x

fSpecialized6 x = square (x * (square x))
