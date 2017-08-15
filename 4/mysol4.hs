type Number = (Int, [Int])

toInt :: Number -> Int
toInt (base, digits) = sum (zipWith (*) (1:iterate (\x->base*x) base) digits)

concatMap' f = foldr aux e
 where 
	aux = \x xs -> f x ++ xs
	e = []

f [] = []
f (x:xs) = reverse x ++ f xs