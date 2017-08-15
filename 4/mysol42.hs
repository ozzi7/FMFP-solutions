type Number = (Int, [Int])

toInt :: Number -> Int
toInt (base, d) = sum (zipWith (*) (1:iterate (\x -> base*x) base) d)