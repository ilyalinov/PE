import qualified Data.Text as T

square :: Int -> Int
square x = x * x

f :: Int -> Int -> Int
f x 0  = 1
f x n | even n    = square (f (quot n 2) x)
      | otherwise = (f (n - 1) x) * x

fSpecialized6 x = square (x * (square x))


generatingExtension n = "fSpecialized" ++ show n ++ " x = "++ loop n where
    loop 0 = "1"
    loop n | even n    = "(square " ++ loop (n `quot` 2) ++ ")"
           | otherwise = "(" ++ loop (n - 1) ++ " * x)"
    
