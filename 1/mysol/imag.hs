type Complex = (Double, Double)

re :: (Double,Double) -> Double
re (x, y) = x

im :: Complex -> Double
im (x, y) = y

conj :: Complex -> Complex
conj (x,y) =
 (x,(-y))

mult :: Complex -> Complex -> Complex
mult (x,y) (u,v) =
 (x*u-y*v,y*u+x*v)

main :: IO ()
main = do
 putStrLn "real:"
 a <- getLine
 putStrLn "imag:"
 b <- getLine
 let quad = mult (read a,read b) (read a,read b)
 putStrLn ("in quadrat: " ++ (show quad))