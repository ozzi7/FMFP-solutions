{-
Exercise Sheet 1 
Author: John Do (the 1st)
Tutor:  Alonzo Church

Assignment 1
------------

-}

myGcd :: Int -> Int -> Int
myGcd x y
  | x == y = x
  | y > x = myGcd y x
  | otherwise = myGcd (x-y) y

{-
(a)

myGcd 139629 83496 evaluates to 21.

If one of the arguments is negative then myGcd does not terminate in
general. However, if both arguments are equal then myGcd terminates
because of the first case split "| x == y = ...".  If the arguments
are not equal and one is negative then we can assume without loss of
generality that the first argument is larger than the second argument
because of the second case split "| x > y = ...". Hence, y is
negative.  In the third case split we apply myGcd to (x-y) and y. This
means that the first argument of myGcd becomes larger and the second
argument remains the same.  From this it follows that myGcd does not
terminate.

Similar arguments as above can be given if one of the arguments is 0.
-}

gcdInt :: Int -> Int -> Int
gcdInt x y = 
  let (x',y') = (if x >= 0 then x else -x, if y >= 0 then y else -y)
  in myGcd x' y'

{-
gcdInt does not terminate for all inputs. For example, gcdInt 0 4 does
not terminate.

(b)
-}

gcdInt1 :: Int -> Int -> Int
gcdInt1 x y = myGcd (abs x) (abs y)

-- An alternative solution is

gcdInt2 :: Int -> Int -> Int
gcdInt2 = gcd

{-
Note that gcdInt2 0 4 terminates. 

(c)
-}

myGcdDouble :: Double -> Double -> Double
myGcdDouble x y
  | x == y    = x
  | y > x     = myGcdDouble y     x
  | otherwise = myGcdDouble (x-y) y

{-
myGcdDouble 3.6 7.2 evaluates to 3.6 and myGcdDouble 3.6 7.1999999
does not terminate, because floating point arithmetic makes equality 
comparisons very fragile, so better avoid them.



Assignment 2
------------

...
-}