otp :: [Bool] -> [Bool] -> [Bool]
otp (m:ms) (k:ks) = (m /= k) : otp ms ks
otp _ _ = []

otp2 :: [Bool] -> [Bool] -> [Bool]
otp2 ms ks = zipWith (/=) ms ks

otp3 :: [Bool] -> [Bool] -> [Bool]
otp3 ms ks = map (\(x,y) -> x /= y) (zip ms ks)

prime :: Int -> Bool
prime n = [1,n] == [x | x <- [1..n], n `mod` x == 0]

primeList :: Int -> [Int]
primeList m = [x | x <- [1..m], prime x == True]

firstMPrimes :: Int -> [Int]
firstMPrimes m = count 0 m
 where
 	count c m
	 | m == 0 = []
 	 | (prime c) = c:(count (c+1) (m-1)) 
	 | otherwise = count (c+1) m

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (delete xs 1)) (mergeSort (delete xs 2))

delete :: [Int] -> Int -> [Int]
delete [] _= []
delete (x:xs) 1 = x:(delete xs 2)
delete (x:xs) 2 = (delete xs 1)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
 | x<y = x:merge xs (y:ys)
 | otherwise = y:merge (x:xs) ys

split :: Char -> String -> [String]
split sep s = aux s ""
 where
	aux [] w = [w]
	aux (c:cs) w
	 | c == sep = w:aux cs ""
	 | otherwise = aux cs (w++[c])
	
isASpace :: Char -> Bool
isASpace c = c == ' '

toWords :: String -> [String]
toWords s = split ' ' s

countWords :: String -> Int
countWords s = foldr (\x y -> y + 1) 0 (split ' ' s)















