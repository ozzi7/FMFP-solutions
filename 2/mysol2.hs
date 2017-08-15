improve :: Double -> Double -> Double
improve x yApprox = 
 (yApprox + x / yApprox) / 2

goodEnough :: Double -> Double -> Bool
goodEnough yApproxPrior yApproxCurrent
 | abs((yApproxCurrent - yApproxPrior) / yApproxPrior) < 0.001 =       True
 | otherwise = False

eps :: Double
eps = 0.001

y0 :: Double
y0 = 1.0

root :: Double -> Double
root x = iter y0
	where iter y
  		| goodEnough y y' = y'
  		| otherwise = iter y'
  		where y' = improve x y 

main :: IO ()
main = do
	putStrLn "Enter x: "
	input <- getLine
	let x = read input
	if x <= 0.0
		then return ()
		else do
			let r = root x
			putStrLn("result bitches: " ++ (show r))
			main

cntChange :: Int -> Int
cntChange a = change a 500
	where change a d
		| a==0 = 1
		| a<0 || d== (-1) = 0
		| otherwise = change a (predCoin d) + change (a-d) d

predCoin :: Int -> Int
predCoin 500 = 200
predCoin 200 = 100    
predCoin 100 = 50  
predCoin 50 = 20
predCoin 20 = 10
predCoin 10 = 5
predCoin _  = -1





