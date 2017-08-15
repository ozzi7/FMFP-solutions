data Expr = Varx | ConstE
	| Num Double
	| Mul Expr Expr
	| Add Expr Expr
	| Exp Expr

instance Show Expr where
	show (Mul e1 e2) = ((show e1) + "*" + (show e2))
	show Varx = "x"
	show (Num x) = show x

diff :: Expr -> Expr
diff (Num a) = (Num 0.0)
diff (ConstE) = (Num 0.0)
diff (Add e1 e2) = (Add (diff e1) (diff e2))
diff (Exp e1) = (Mul (diff e1) (Exp e1))
diff (Varx) = (Num 1.0)
