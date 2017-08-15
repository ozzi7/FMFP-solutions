data Aexp = Bin Op Aexp Aexp | Var String | Num Integer
data Op = Add | Sub | Mul
data Bexp = Or Bexp Bexp | And Bexp Bexp | Not Bexp | Rel Rop 
	Aexp Aexp
data Rop = Eq | Neq | Le | Leq | Ge | Geq
type State = (String -> Integer)

evalAexp :: Aexp -> State -> Integer
evalAexp (Num n) _ = n
evalAexp (Var s) sigma = s
evalAexp (Bin Add e1 e2) sigma = (evalAexp e1 sigma) + 	(evalAexp 	e2 sigma)
evalAexp (Bin Sub e1 e2) sigma = (evalAexp e1 sigma) - 	(evalAexp e2 sigma)
evalAexp (Bin Mul e1 e2) sigma = (evalAexp e1 sigma) * 	(evalAexp e2 sigma)

evalBexp :: Bexp -> State -> Bool
evalBexp (Or b1 b2) sigma = (evalBexp b1 sigma) || (evalBexp 	b2 	sigma)
evalBexp (And b1 b2) sigma = (evalBexp b1 sigma) &&(evalBexp 	b2 	sigma)
evalBexp (Not b1) sigma = not (evalBexp b1 sigma)
evalBexp (Rel operator e1 e2) sigma = (evalOp op) (evalAExp 	e1 sigma) (evalAexp e2 sigma)
	where evalOp Eq = (==)
		evalOp Neq = (!=)
		evalOp Le = (<)
		evalOp Leq = (<=)
		evalOp Ge = (>)
		evalOp Geq = (>=)