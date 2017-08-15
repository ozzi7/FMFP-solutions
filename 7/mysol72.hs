match :: String -> Bool
match string = check [] string
	where 
		check stack [] = (null stack)
		check stack (s:ss)
			| s == ']' && ((last stack) == '[') = check (init 					stack) ss
			| s == '}' && ((last stack) == '{') = check (init 					stack) ss
			| s == ')' && ((last stack) == '(') = check (init 					stack) ss
			| s == '(' = check (stack++ ['(']) ss
			| s == '{' = check (stack++ ['{']) ss
			| s == '[' = check (stack++ ['[']) ss
			| otherwise = check stack ss

risers :: Ord a => [a] -> [[a]]
risers [] = [[]]
risers (x:xs) = ris [] (x:xs)
	where
		ris curr [] = [curr]
		ris curr (x:xs)
		 | curr == [] = ris [x] xs
		 | (last curr) <= x = ris (curr ++ [x]) xs
		 | otherwise = [curr] ++ ris [] (x:xs)

type Poly a = [(Integer, [a])]

evalPoly :: (a -> Integer) -> Poly a -> Integer
evalPoly f p = sum [ a*product(map f xs) | (a,xs) <- p]

data SymbExpr a = Var a | Lit Integer | Add (SymbExpr a) (SymbExpr a) | Mul (SymbExpr a) (SymbExpr a) 

foldSymbExpr :: (a-> b) -> (Integer -> b) ->( b->b->b)->
	(b->b->b) -> SymbExpr a -> b
foldSymbExpr var ass add mul = go
	where
		go (Var a) = var a
		go (Lit l) = ass l
		go (Add s1 s2) = add (foldSymbExpr s1) (foldSymbExpr s1)