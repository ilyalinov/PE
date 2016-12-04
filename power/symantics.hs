class Symantics repr where
    int :: Int -> repr Int
    bool :: Bool -> repr Bool

    lam :: (repr a -> repr b)
    app :: repr (a -> b) -> repr a -> repr b
    fix :: (repr a -> repr a) -> repr a

    add :: repr Int -> repr Int -> repr Int
    mult :: repr Int -> repr Int -> repr Int
    leq :: repr Int -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a
