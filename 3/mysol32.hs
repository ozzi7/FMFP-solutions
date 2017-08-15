prime :: Int -> Bool
prime n
	| [1,n] == [x | x <- [1..n], n `mod` x == 0] = True
	| otherwise = False 

primes :: Int -> [Int]
primes n
	| n >=2 = primes (n-1) ++ (current n)
	| otherwise = []
	where
		current n
			| prime n = [n]
			| otherwise = []

split :: Char -> String -> [String]
split sep s = aux s ""
	where 
		aux [] w = [w]
		aux (c:cs) w 
			| c == sep = w:aux cs ""
			| otherwise = aux cs (w++[c])