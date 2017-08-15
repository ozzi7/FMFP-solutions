match :: String -> Bool
match xs = check (getList xs []) []

check :: String -> String -> Bool
check [] ys
	| ys == [] = True
	| otherwise = False
check (x:xs) ys 	
	| x == '(' || x == '{' = check xs (ys++[x])
	| ys == [] = False
	| x == ')' && last ys == '(' = check xs (init ys)
	| x == '}' && last ys == '{' = check xs (init ys)
	| otherwise = False
getList :: String -> String -> String
getList [] ys = ys
getList (x:xs) ys
	| x== '{' || x == '}' || x == '(' || x == ')' = getList xs 										(ys++[x])
	| otherwise = getList xs ys

risers :: Ord a => [a] -> [[a]]
risers = go [[]]

go :: Ord a => [[a]] -> [a] -> [[a]]
go ys [] = ys
go [[]] (x:xs) = go [[x]] xs
go (y:ys) (x:xs)
	| (last y) <= x = go ([y++[x]]++ys) xs 
	| otherwise = go ([[x]]++(y:ys)) xs

type Poly a = [(Integer,[a])]

evalPoly :: (a->Integer) -> Poly a -> Integer
evalPoly f p = sum [a*product (map f xs) | (a,xs) <- p]

data SymbExpr a = Var a | Lit Integer | Add (SymbExpr a) (SymbExpr a) | Mul (SymbExpr a) (SymbExpr a)

foldSymbExpr :: (a->b) -> (Integer -> b) -> (b->b->b)->(b->b->b)->SymbExpr a -> b
foldSymbExpr fVar fLit fAdd fMul =
	go
	where
		go (Var x) = fVar x
		go (Lit i) = fLit i
		go (Add (l) (r)) = fAdd (go l) (go r)
		go (Mul l r) = fMul (go l) (go r)














