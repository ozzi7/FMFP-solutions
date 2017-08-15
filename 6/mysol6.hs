-- exercise 3
data Expr = Varx | ConstE | Add Expr Expr | Mul Expr Expr
		| Exp Expr | Num Double

instance Show Expr where
	show (Mul e1 e2) = "(" ++ (show e1) ++ "*" ++ (show e2) 					++ ")"
	show (Add e1 e2) = "(" ++ (show e1) ++ "+" ++ (show e2) 					++ ")"
	show (Exp e) = "e^(" ++ (show e) ++ ")"
	show Varx = "x"
	show ConstE = "e"
	show (Num x) = show x

derivate :: Expr -> Expr
derivate Varx = Num 1
derivate (Mul e1 e2) = Add (Mul (derivate e1) e2) (Mul e1  (derivate e2))

-- rest omitted, no challenge

-- exercise 4
-- the idea is always have the same number of queens as the -- number of fields in a row.. not clear in exercise

generate :: Int -> [[Int]]
generate n = gen n
	where
		gen 0 = [[]]
		gen k = [q:qs | q <- [1..n],qs <- gen (k-1)]

test :: [Int] -> Bool
test [] = True
test (q:qs) = row q qs && diag q (zip [1..] qs) && test qs
	where
		row q = all (/=q)
		diag q [] = True
		diag q ((colDiff, row):qs) = (abs(q-row) /= 								colDiff) && diag q qs

naivequeens n = filter test $ generate n