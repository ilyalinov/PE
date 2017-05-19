module Test where

import Data.Char

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f1 f2 xs = 
    let or' = \x -> (f1 x) || (f2 x)
    in filter or' xs
