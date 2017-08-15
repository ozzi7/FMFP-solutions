data Aexp = Binop Op Aexp Aexp | Var String | Num Integer
data Op = Add | Sub | Mul
data Bexp = And Bexp Bexp | Or Bexp Bexp | Not Bexp | Rel Rop 	Aexp
data Rop = Eq | Neq | Le | Leq | Ge | Geq

type State = (String -> Integer)

evalAexp :: Aexp -> State -> Integer
evalAexp
 | (Binop Add a1 a2) state = (evalAexp a1 state) +  (evalAexp a2 state)
 | (Var string) state = state string
 | (Num n) _ = n

evalBexp :: Bexp -> State -> Bool
evalBexp (And b1 b2) state = (evalBexp b1 state) && (evalBexp b2 state) 