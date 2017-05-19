oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs = 
    let oddsOnlyUtil [] ys = ys
        oddsOnlyUtil (x : t) ys | odd x = x : oddsOnlyUtil t ys
                                | even x = oddsOnlyUtil t ys
    in oddsOnlyUtil xs []
