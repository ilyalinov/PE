weightCheck :: Double -> Double -> String
weightCheck weight height
    | x <= 18.5 = "thin!"
    | x <= 25.0 = "you are just fine!"
    | x <= 30.0 = "fatty :3"
    | otherwise = "fat pig!"
    where x = weight / height ^ 2

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT

head' :: [a] -> a
head' xs =
    case xs of
    [] -> error "list is empty!"
    (x:_) -> x
