generate :: Int -> [[Int]]
generate n = gen n
	where
		gen 0 = [[]]
		gen k = [q:qs | q <- [1..n],qs <- gen (k-1)]
 
test :: [Int] -> Bool
test [] = True
test (x:[]) = True
test (x:xs) = test2 x xs && test xs
	where test2 x (xs:xss)
		| x /= xs && xss == [] = True
		| x /= xs && xss /= [] = test2 x xss
		| otherwise = False
naivequeens :: Int -> [[Int]]
naivequeens n = filter test $ generate n

checkdiag :: [Int] -> Bool
checkdiag [] = True
checkdiag (x:xs) n = checkminus n (x:xs) && checkplus (x:xs) &&
	where 
		checkminus  n = True
		checkplus  n = True
































