data Odd = Odd Integer deriving (Eq,Show)

instance Enum Odd where
    succ (Odd x)                                            = Odd (x + 2)
    pred (Odd x)                                            = Odd (x - 2)
    toEnum x                                                = Odd (toInteger x)
    fromEnum (Odd x)                                        = fromIntegral x
    enumFrom (Odd x)                                        = map Odd [fromIntegral x, fromIntegral (x + 2)..]
    enumFromThen (Odd x) (Odd y)                            = map Odd [fromIntegral x, fromIntegral y..]
    enumFromTo (Odd x) (Odd y)                 = map Odd [fromIntegral x, fromIntegral (x + 2)..fromIntegral y]
    enumFromThenTo (Odd x) (Odd y) (Odd z)     = map Odd [fromIntegral x, fromIntegral y..fromIntegral z]

test0 = succ (Odd 1) == (Odd 3)
test1 = pred (Odd 3) == (Odd 1)
-- enumFrom
test2 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- enumFromTo
-- -- По возрастанию
test3 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test4 = (take 3 $ [Odd 7..Odd 1]) == []
-- enumFromThen
-- -- По возрастанию
test5 = (take 3 $ [Odd 1, Odd 3 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test6 = (take 3 $ [Odd 3, Odd 1 ..]) == [Odd 3,Odd 1,Odd (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 =([Odd 1, Odd 5 .. Odd 7]) == [Odd 1,Odd 5]
-- -- По убыванию
test8 =([Odd 7, Odd 5 .. Odd 1]) == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- x1 < x3 && x1 > x2
test9 =([Odd 7, Odd 5 .. Odd 11]) == []
-- -- x1 > x3 && x1 < x2
test10 =([Odd 3, Odd 5 .. Odd 1]) == []

allTests = zip [0..] [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
