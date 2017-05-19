groupElems :: Eq a => [a] -> [[a]]
groupElems xs =
    let helper [] ys                         = ys
        helper (x:xs) []                     = helper xs [[x]]
        helper (x:xs) (y:ys) | (head y) == x = helper xs ((x:y):ys)
                             | otherwise     = helper xs ((x:[]):(y:ys))
    in helper (reverse xs) []
