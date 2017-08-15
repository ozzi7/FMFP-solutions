***************************************************************************
*** SIMPLE IMP INTERPRETER(S) *********************************************
***************************************************************************

Authors: Ben Bitdiddle, Eva LaTour, and others
Version: 0


The IMP interpreter will make use of the following predefined Haskell
functions, which are taken from the specified standard Haskell
libraries.

> import Data.List (sort, nub, intersperse)
> import Data.Char (isLower, isDigit)
> import System.Environment (getArgs, getProgName)


ALGEBRAIC DATA TYPES FOR IMP 
****************************

We represent arithmetic expressions as elements of the following
algebraic data type. Recall that an arithmetic expression is either an
(non-negative) integer, a variable, or it is constructed from two
arithmetic expressions with an arithmetic operator (+, -, *, /, mod).

> data Op = Add | Sub | Mul | Div | Mod
> data Aexp = Num Integer
>           | Var String
>           | BinOp Op Aexp Aexp

We use the Haskell type Integer to handle arbitrary-precision integers
in IMP. The range of the type Int is limited to [-2^29, 2^29 - 1].

Similar to arithmetic expressions, the elements of the following
algebraic data type represent Boolean expressions.

> data ROp = Eq | Neq | Le | Leq | Ge | Geq
> data Bexp = Rel ROp Aexp Aexp
>           | Not Bexp 
>           | Or Bexp Bexp
>           | And Bexp Bexp


Recall that an IMP program is either 
(1) the command SKIP, 
(2) an assignment x:=e, where x is a variable and e an arithmetic 
    expression, 
(3) the sequential composition s1;s2 of two IMP programs s1 and s2, 
(4) an if-then-else statement IF b THEN s1 ELSE s2 END, where
    b is a Boolean expression, and s1 and s2 are IMP programs, or
(5) a while loop WHILE b DO s END, where b is a Boolean expression 
    and s an IMP program.
We represent IMP programs (or statements) as elements of the algebraic
data type Stm:

> data Stm = Skip 
>          | Assign String Aexp 
>          | SeqCompos Stm Stm
>          | If Bexp Stm Stm
>          | While Bexp Stm


For pretty printing of IMP programs, we put the algebraic data types
Aexp, Op, Bexp, and Stm into the type class Show. The binary Boolean
operators and the arithmetic operators are written infix.

> instance Show Aexp where
>     show (Num n) = show n
>     show (Var s) = s
>     show (BinOp op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"

> instance Show Op where
>     show Add  = "+"
>     show Sub = "-"
>     show Mul  = "*"
>     show Div = "/"
>     show Mod  = "MOD"

> instance Show ROp where
>     show Eq  = "="
>     show Neq = "#"
>     show Le  = "<"
>     show Leq = "<="
>     show Ge  = ">"
>     show Geq = ">="

> instance Show Bexp where
>     show (Rel op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
>     show (Not b) = "(NOT " ++ show b ++ ")"
>     show (And b1 b2) = "(" ++ show b1 ++ " AND " ++ show b2 ++ ")"
>     show (Or b1 b2) = "(" ++ show b1 ++ " OR " ++ show b2 ++ ")"


To increase readability of IMP programs, we separate composed
statements by \n, and we indent the body of a while loop and the
branches of an if statement.

> instance Show Stm where
>     show = indent ""
>         where indent ind Skip = ind ++ "SKIP"
>               indent ind (Assign x e) = ind ++ x ++ " := " ++ show e
>               indent ind (SeqCompos s1 s2) = (indent ind s1) ++ ";\n" ++ 
>                                              (indent ind s2)
>               indent ind (If b s1 s2) = ind ++ "IF " ++ show b ++ " THEN\n" ++ 
>                                         (indent ind' s1) ++ "\n" ++
>                                         ind ++ "ELSE\n" ++
>                                         (indent ind' s2) ++ "\n" ++
>                                         ind ++ "END"
>                   where ind' = ind ++ "  "
>               indent ind (While b s) = ind ++ "WHILE " ++ show b ++ " DO\n" ++
>                                        (indent ind' s) ++ "\n" ++
>                                        ind ++ "END"
>                   where ind' = ind ++ "  "


We define some examples of IMP programs:

> impFoo = 
>     Assign "y" (Num 1)

> impGoo = 
>     SeqCompos (Assign "y" (Num 1))
>               (Assign "x" (BinOp Add (Var "y") (Num 3)))

> impFactorial = 
>     SeqCompos 
>       (Assign "y" (Num 1))
>       (While (Not (Rel Eq (Var "x") (Num 1)))
>              (SeqCompos 
>                (Assign "y" (BinOp Mul (Var "y") (Var "x")))
>                (Assign "x" (BinOp Sub (Var "x") (Num 1)))))

> impRoot = 
>     SeqCompos 
>       (Assign "z" (Num 0))
>       (SeqCompos 
>         (Assign "v" (Num 0))
>         (While (Rel Le (Var "v") (Var "x"))
>                (SeqCompos 
>                  (Assign "v" (Num 1))
>                  (SeqCompos 
>                    (Assign "i" (Num 0))
>                    (SeqCompos 
>                      (While (Rel Le (Var "i") (Var "y"))
>                             (SeqCompos 
>                               (Assign "v" (BinOp Mul (Var "v") (BinOp Add (Var "z") (Num 1))))
>                               (Assign "i" (BinOp Add (Var "i") (Num 1)))))
>                      (If (Rel Leq (Var "v") (Var "x"))
>                          (Assign "z" (BinOp Add (Var "z") (Num 1)))
>                          Skip))))))


STATES 
******

Recall that a state is a function that maps variables to integers.
We model a state by the data type

> data State = VarAssign (String -> Integer)

Often variables are initially set to 0, that is, IMP programs start in
the initial state where each variable is assigned to 0. We define

> allZeroState = VarAssign (\x -> 0) 

To pointwise alter a state, we define the function updateState. In the
lecture and on the assignment sheet, we write sigma[y |-> n], which is
equivalent to updateState sigma y n in Haskell.

> updateState :: State -> String -> Integer -> State
> updateState (VarAssign val) x n = VarAssign val'
>     where val' y 
>               | x == y = n
>               | otherwise = val y

Since there are infinitely many variables, we cannot put the data type
State into the type class Show. Instead, we define a function
showState that converts a state into a string. The string only
contains the "relevant" variables and their values. For example, if an
IMP program only contains the variables x1, ..., xn, then we are only
interested in the values of x1, ..., xn in a state. In this sense, the
variables x1, ..., xn are the relevant variables with respect to that
IMP program.

> showState (VarAssign val) vars = 
>     "[ " ++ (concat (intersperse ", " mapping)) ++ " ]"
>     where vars' = (sort . nub) vars
>           vals = map (show . val) vars' 
>           mapping = zipWith (\x y -> x ++ " |-> " ++ y) vars' vals

The following functions extract the variables that occur in arithmetic
expressions, Boolean expressions, and IMP programs.

> varsAexp e = nub (vars e)
>     where vars (Num _)     = []
>           vars (Var s)     = [s]
>           vars (BinOp op e1 e2) = vars e1 ++ vars e2

> varsBexp b = nub (vars b)
>     where vars (Rel _ e1 e2) = varsAexp e1 ++ varsAexp e2
>           vars (Not b)       = vars b
>           vars (Or b1 b2)    = vars b1 ++ vars b2
>           vars (And b1 b2)   = vars b1 ++ vars b2

> varsStm s = nub (vars s)
>     where vars (Skip)            = []
>           vars (Assign x e)      = x : varsAexp e
>           vars (SeqCompos s1 s2) = vars s1 ++ vars s2
>           vars (If b s1 s2)      = varsBexp b ++ vars s1 ++ vars s2
>           vars (While b s)       = varsBexp b ++ vars s


EVALUATING ARITHMETIC AND BOOLEAN EXPRESSIONS 
*********************************************

Recall the definitions of caligraphicA[[e]]sigma and
caligraphicB[[b]]sigma from the lecture, where e is an arithmetic
expression, b a Boolean expression, and sigma a state. In Haskell, we
define the functions caligraphicA and caligraphicB as follows:

> evalAexp :: Aexp -> State -> Integer
> evalAexp (Num n)     _               = n
> evalAexp (Var x)     (VarAssign val) = val x
> evalAexp (BinOp op e1 e2) sigma = (evalOp op) (evalAexp e1 sigma) (evalAexp e2 sigma)
>   where evalOp Add  = (+)
>         evalOp Sub = (-)
>         evalOp Mul  = (*)
>         evalOp Div = div
>         evalOp Mod  = mod

Note that evalAexp does not take care of division by 0 when evaluating
an arithmetic expression.

> evalBexp :: Bexp -> State -> Bool
> evalBexp (Rel op e1 e2) sigma = 
>     (evalOp op) (evalAexp e1 sigma) (evalAexp e2 sigma)
>   where evalOp Eq  = (==)
>         evalOp Neq = (/=)
>         evalOp Le  = (<)
>         evalOp Leq = (<=)
>         evalOp Ge  = (>)
>         evalOp Geq = (>=)
> evalBexp (Not b)     sigma = not (evalBexp b sigma)
> evalBexp (Or b1 b2)  sigma = (evalBexp b1 sigma) || (evalBexp b2 sigma)
> evalBexp (And b1 b2) sigma = (evalBexp b1 sigma) && (evalBexp b2 sigma)


CONFIGURATIONS
**************

Recall from the lecture that a configuration is either final or
non-final.  A final configuration consists of a state. A non-final
configuration consists of an IMP program and a state on which we start
to run the given IMP program. We define the following data type:

> data Config = NonFinal Stm State | Final State

Later, we will make use of the following simple auxilary functions for
configurations.

> getStm (NonFinal s _) = s
> getStm (Final _) = error "No statement since final state"

> getState (NonFinal _ sigma) = sigma
> getState (Final sigma) = sigma

> isFinal (Final _) = True
> isFinal (NonFinal _ _) = False



NATURAL SEMANTICS FOR IMP
*************************

Recall from the lecture that the transition relation --> for the
natural semantics (NS) consists of pairs (source,target), where source
is an element of type (Stm,State) and target is an element of type
State. The relation --> is specified by rules of the form

   tr1    ...    trn
  ------------------- if cond
          tr

where tr1, ..., trn, and tr are transitions. The meaning of such a
rule is as follows. If there are transitions tr1, ..., trn, and the
condition cond is true then we also have a transition tr.  tr1, ...,
trn are called premises. A rule without premises is an axiom.

We encode the rules from the lecture for the natural semantics in the
function transNS. Since --> is deterministic (this has been proved in
the lecture), transNS is a partial function of type Config -> Config.
An alternative type for the function is Config -> State.  However,
below we define the corresponding function for SOS. There, the target
of a transition can also be an element of (Stm,State).

> transNS :: Config -> Config

For the IMP program SKIP, we have the axiom


  ------------------------
   (SKIP,sigma) --> sigma

> transNS (NonFinal Skip sigma) = 
>     Final sigma

For an assignment x:=e, we have the axiom


  --------------------------------------------------------
   (x:=e, sigma) --> sigma[x |-> caligraphicsA[[e]]sigma]

> transNS (NonFinal (Assign x e) sigma) =
>     Final (updateState sigma x (evalAexp e sigma))

For the sequential composition s1;s2, we have the rule

   (s1,sigma) --> sigma'     (s2,sigma') --> sigma''
  ---------------------------------------------------
             (s1;s2,sigma) --> sigma''

> transNS (NonFinal (SeqCompos s1 s2) sigma) =
>     Final sigma''
>     where Final sigma' = transNS (NonFinal s1 sigma)
>           Final sigma'' = transNS (NonFinal s2 sigma')

For an if statement IF b THEN s1 ELSE s2 END, we have the rule

               (s1,sigma) --> sigma'
  ---------------------------------------------- if B[[b]]sigma=tt
   (IF b THEN s1 ELSE s2 END, sigma) --> sigma'

and the rule

               (s2,sigma) --> sigma'
  ---------------------------------------------- if B[[b]]sigma=ff
   (IF b THEN s1 ELSE s2 END, sigma) --> sigma'

> transNS (NonFinal (If b s1 s2) sigma) 
>     | evalBexp b sigma = Final sigma'
>     where Final sigma' = transNS (NonFinal s1 sigma)

> transNS (NonFinal (If b s1 s2) sigma) 
>     | not (evalBexp b sigma) = Final sigma'
>     where Final sigma'  = transNS (NonFinal s2 sigma)


For a while loop WHILE b DO s END, we have the rule

   (s,sigma) --> sigma'  ((WHILE b DO s END, sigma') --> sigma''
  --------------------------------------------------------------- if B[[b]]sigma=tt
                 (WHILE b DO s END, sigma) --> sigma''

and the axiom


  ------------------------------------- if B[[b]]sigma=ff
   (WHILE b DO s END, sigma) --> sigma


> transNS (NonFinal (While b s) sigma)
>     | evalBexp b sigma = Final sigma''
>     where Final sigma' = transNS (NonFinal s sigma)
>           Final sigma'' = transNS (NonFinal (While b s) sigma')

> transNS (NonFinal (While b s) sigma)
>     | not (evalBexp b sigma) = Final sigma


Executing an IMP program s from the initial state sigma under the
natural semantic is done by the following function. Note that under
the natural semantics, an IMP program goes to a final configuration,
i.e., a configuration of the form
  Final sigma'.
However, there might be no final configuration, e.g., when a while
loop does not terminate. 

Since we are interested in the values of the variables in the final
configuration, the function returns the state of the final
configuration. Actually, it converts the state into a string that
shows the values of the relevant variables with respect to the given
IMP program.

> runNS sigma s = showState sigma' (varsStm s)
>     where Final sigma' = transNS (NonFinal s sigma)

> run s = sigma'
>     where Final sigma' = transNS (NonFinal s allZeroState)

> result s = sigma "result"
>     where VarAssign sigma = run s

STRUCTURAL OPERATIONAL SEMANTICS FOR IMP
****************************************

See next the lecture and the next exercise sheet.


EXAMPLES OF THE EVALUATION OF IMP PROGRAMS
******************************************

We can run the program impFactorial, which computes the factorial of
the value that is stored in the variable x in the initial state, under
the natural and the SOS semantics. For the natural semantics, we type

  runNS aState impFactorial

in HUGS and 

  runSOS aState impFactorial 

when we want to use the SOS semantics. For the definition

> aState = updateState (allZeroState) "x" 6

HUGS outputs in both cases

  "[ x |-> 1, y |-> 720 ]"

That is, in the final state, the variable x stores the value 1 and
the variable y stores the value 720, which is the factorial of
6---the value of the variable x in the initial state. For

  runNS anotherState impFactorial

and

  runSOS anotherState impFactorial 

with

> anotherState = updateState (allZeroState) "x" 42

the output is

  "[ x |-> 1, y |-> 1405006117752879898543142606244511569936384000000000 ]"

A final test case: with

> yetAnotherState = updateState (allZeroState) "x" 0815

HUGS outputs

  "[ x |-> 1, y |-> 31489191993362257125490308208602244007311288533890839299894311565916854113687290435932401338666875633312919290944827631760199749515575132016115238685032970341963155422292652366083333720526026513125951657908667648976338753003305487163499325864282694748450435807260031594816316259926744077276979780079997002387024547591864894198298034558743789726870366312123863059157455878713911566480748279627080103897938833567903413291273751058172891965906950728102643889458038974202407079176981407271858071684951457556593249139011847918334839424414293807170262410534792481252421322217288751579288290766519384914922118862510221666179701590893489771066944622901971135963923713642181227023461653596198147684347138263117240019330640856576022738699443275827817387405226045469753270781455448003903640611567035387439137225924447523170341920236780449785112383107834398223839435309326487150730319082867313305701440293538817384242101248939821575496751208969388104293337121103183482020412630389683183765161646804269197174435783110172023592980690600603599860932553879332457025919852151196633582022984792803350479507646999056418927496157040164856578059306430470366945006539562161071444337652129875000008763393245247756697638415093210240768361829171263829700946274227565825564620854191604009374377781350174282098498983293142727618466349870876923278104674304329157859305211081799004811215139078867474243390095652767024073222702539255686699879535881343488812862435019175120933191260928115106385253630937268179758176201551825521487899881115472820272463010106003393019826012635357422654652815379375279032560969544671558059656565516538949949783314819116187559184648294055337498575844317448877658866427259294242738467281767222379992629694699798556323959059448030731310155218259709014622474759940564892916653953558941802264477653599451570231205980882862080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 ]"

> someState = updateState (anotherState) "y" 3


PARSING IMP PROGRAMS
********************

We use the parser combinators from the first part of the lecture. See
the FP part for a description of these functions and data types.

> data Parser a = Prs (String -> [(a,String)])

> parse :: Parser a -> String -> [(a,String)]
> parse (Prs p) inp = p inp

> completeParse :: Parser a -> String -> a
> completeParse p inp 
>   | null results = error "Parse unsuccessful"
>   | otherwise    = head results
>   where results = [val | (val,[]) <- parse p inp]

> instance Monad Parser where
>     return x = Prs (\inp -> [(x,inp)])
>     p >>= gen = Prs (\inp -> [(val',out') | (val,out) <- parse p inp, 
>                                             (val',out') <- parse (gen val) out])
>     fail = const failure

> (|||) :: Parser a -> Parser a -> Parser a
> p ||| q = Prs (\inp -> parse p inp ++ (parse q inp))

> (+++) :: Parser a -> Parser a -> Parser a
> p +++ q = Prs (\s -> case parse p s of
>                        [] -> parse q s
>                        res -> res)

> many :: Parser a -> Parser [a]      
> many p = many1 p +++ return []    

> many1 :: Parser a -> Parser [a]
> many1 p = do val <- p               
>              vals <- many p
>              return (val:vals)

> failure = Prs (\inp -> [])          

> item :: Parser Char
> item = Prs (\inp -> case inp of
>                       "" -> []
>                       (x:xs) -> [(x,xs)])

> sat  :: (Char -> Bool) -> Parser Char
> sat p = do x <- item 
>            if p x then return x else failure

> char  :: Char -> Parser Char
> char x = sat (==x)

> string :: String -> Parser String
> string "" = return ""
> string (x:xs) = do char x 
>                    string xs 
>                    return (x:xs)

> spaces = many (sat (\c -> c==' ' || c=='\n' || c=='\t'))

> identifier = do id <- many1 (sat isLower)
>                 return id

> token s = do spaces
>              t <- string s
>              spaces
>              return t

> numPos :: Parser Integer
> numPos = do digits <- many1 (sat isDigit) 
>             return (read digits)
> numNeg :: Parser Integer
> numNeg = do char '-' 
>             n <- numPos 
>             return (-n)
> num :: Parser Integer
> num = numPos ||| numNeg


We parse arithmetic expression by the grammar

  aexp ::= aatom | aatom aop aexp
  aatom ::= identifier | integer | '(' aexp ')'
  aop ::= '+' | '-' | '*' | '/' | 'MOD'

Note that the arithmetic operators are right associative, e.g. 4-2-1
means 4-(2-1).  Furthermore, note that * does not bind stronger than +
in our grammar.

> aexp = aatom ||| aexpComb
>     where aexpComb = do spaces
>                         a <- aatom
>                         bop <- aop
>                         e <- aexp
>                         spaces
>                         return (BinOp bop a e)
> aatom = avar ||| aint ||| aexpPar
>     where avar = do spaces
>                     s <- identifier
>                     spaces
>                     return (Var s)
>           aint = do spaces
>                     n <- num
>                     spaces
>                     return (Num n)
>           aexpPar = do token "("
>                        e <- aexp
>                        token ")"
>                        return e
> aop = aopAdd ||| aopSub ||| aopMul ||| aopDiv ||| aopMod
>     where aopAdd = do token "+"
>                       return Add
>           aopSub = do token "-"
>                       return Sub
>           aopMul = do token "*"
>                       return Mul
>           aopDiv = do token "/"
>                       return Div
>           aopMod = do token "MOD"
>                       return Mod

We parse Boolean expressions by the grammar

  bexp ::= batom | 'NOT' bexp | batom 'AND' bexp | batom 'OR' bexp
  batom ::= aexp relop aexp | '(' bexp ')'
  relop ::= '=' | '#' | '<' | '<=' | '>' | '>='

Similar to the arithmetic operators arithmetic, the binary Boolean
connectives are right associative and the connective AND does not bind
stronger than OR.

> bexp = batom ||| bexpNot ||| bexpAnd ||| bexpOr
>     where bexpNot = do token "NOT"
>                        b <- bexp
>                        return (Not b)
>           bexpAnd = do b <- batom
>                        token "AND"
>                        b' <- bexp
>                        return (And b b')
>           bexpOr  = do b <- batom
>                        token "OR"
>                        b' <- bexp
>                        return (Or b b')
> batom = brel ||| bexpPar 
>     where brel    = do e <- aexp
>                        op <- relop
>                        e' <- aexp
>                        return (Rel op e e')
>           bexpPar = do token "("
>                        b <- bexp
>                        token ")"
>                        return b
> relop = relopEq ||| relopNeq ||| relopLe ||| relopLeq ||| relopGe ||| relopGeq
>     where relopEq  = do token "="
>                         return Eq
>           relopNeq = do token "#"
>                         return Neq
>           relopLe  = do token "<"
>                         return Le
>           relopLeq = do token "<="
>                         return Leq
>           relopGe  = do token ">"
>                         return Ge
>           relopGeq = do token ">="
>                         return Geq

IMP programs have the grammar 

  stm ::= satom | stm
  satom ::= 'SKIP' | 
            identifier ':=' aexp | 
            'IF' bexp 'THEN' stm 'ELSE' stm 'END' |
            'WHILE' bexp 'DO' stm 'END'
  stms ::= satom ';' stm

Note that the grammar slightly varies from the grammar given in the
lecture. In particular, keywords (SKIP, IF, THEN, ELSE, WHILE, DO,
END, ...) are upper case and identifiers have to be lower case.

> stm = satom ||| stms
> satom = sskip ||| sassign ||| sif ||| swhile 
>     where sskip = do token "SKIP"
>                      return Skip
>           sassign = do spaces
>                        x <- identifier
>                        token ":="
>                        e <- aexp
>                        spaces
>                        return (Assign x e)
>           sif = do token "IF"
>                    b <- bexp
>                    token "THEN"
>                    s1 <- stm
>                    token "ELSE"
>                    s2 <- stm
>                    token "END"
>                    return (If b s1 s2)
>           swhile = do token "WHILE"
>                       b <- bexp
>                       token "DO"
>                       s <- stm
>                       token "END"
>                       return (While b s)
> stms = do spaces
>           s1 <- satom
>           token ";"
>           s2 <- stm
>           spaces
>           return (SeqCompos s1 s2)


To parse IMP programs we use the function

> parseStm = completeParse stm


IO (MAIN FUNCTION)
******************

You can compile the IMP interpreter with 
  ghc -o simpi simpi.lhs
and run it from a shell (e.g. ./simpi sos factorial.imp).

Alternatively, you can load the literate Haskell file simpi.lhs into
HUGS. In HUGS you can, e.g., start the IMP interpreter in the natural
semantics on the file factorial.imp by
  :main natural factorial.imp

The IMP interpreter takes two command line arguments. The first
argument specifies under which semantics the program should be
executed and the second argument is a file with the IMP program. The
initial state is the state in which all variables have the value 0.

> main = do p <- getProgName       -- get name of the programname
>           args <- getArgs        -- get command line arguments
>           if length args /= 2    -- do some simple checking of arguments
>                  then putStr (usage p) 
>                  else do prg <- readFile (head (tail args))
>                          putStr ((run (head args) (parseStm prg)) ++ "\n")
>           return ()
>     where usage p = "A simple IMP interpreter\n" ++
>                     "usage: " ++ p ++ " ns|sos|sos_verbose filename\n"
>           run "ns"          = runNS allZeroState
>           run "sos"         = error "SOS not yet implemented"
>           run "sos_verbose" = error "SOS not yet implemented"
>           run _             = error "Unknown semantics"


EXAMPLES OF IMP PROGRAMS
************************

> prgFoo = "res := 1"

> prgGoo = "res := (res * n);" ++
>          "n := n - 1"

> prgFactorial = "res := 1; "         ++
>                "WHILE n > 1 DO "    ++
>                "  res := res * n; " ++
>                "  n := n - 1 "      ++
>                "END"

> prgRoot = "z := 0;\n" ++
>           "v := 0;\n" ++
>           "WHILE v < x DO\n" ++
>           "  v := 1;\n" ++
>           "  i := 0;\n" ++
>           "  WHILE i < y DO\n" ++
>           "    v := v * (z+1);\n" ++
>           "    i := i+1\n" ++
>           "  END;\n" ++
>           "  IF v <= x THEN\n" ++
>           "    z := z+1\n" ++
>           "  ELSE\n" ++
>           "    SKIP\n" ++
>           "  END\n" ++
>           "END"

> test1 = "n := 2;\n" ++
>         "b := 1;\n" ++
>         "WHILE n # 0 DO\n" ++
>         "    a := a+n;\n" ++
>         "    b := b*n;\n" ++
>         "    n := n-1\n" ++
>         "END;\n" ++
>         "result := a"

> test2 = "result := 1;\n" ++
>         "n := 10;\n" ++
>         "WHILE n > 1 DO\n" ++
>         "    result := result*n;\n" ++
>         "    n := n-1\n" ++
>         "END"

> test3 = "x := 15648;\ny := 3;\n" ++ prgRoot ++ ";\nresult := z"