module Lambda(
	Term, isSyn, isVar, isNum, isBool, bool, var, syn, num, ab, ap,
	betaReduce,
	stdlib) where

data Term
	= Var String
	| Num Int
	| Boolean Bool
	| Abstr Term Term
	| Ap Term Term
	| Syn String
	| Binop String Term Term
	| Unop String Term
	deriving (Eq)

instance Show Term where
	show = showTerm

isSyn :: Term -> Bool
isSyn (Syn _) = True
isSyn _ = False

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

isNum :: Term -> Bool
isNum (Num _) = True
isNum _ = False

isBool :: Term -> Bool
isBool (Boolean _) = True
isBool _ = False

showTerm :: Term -> String
showTerm (Var name) = name
showTerm (Num n) = show n
showTerm (Boolean b) = if b then "#t" else "#f"
showTerm (Abstr (Var x) t) = "(\\" ++ x ++ ". " ++ show t ++ ")"
showTerm (Ap t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
showTerm (Syn name) = name
showTerm (Binop name t1 t2) = "(@" ++ name ++ " " ++ show t1 ++ " " ++ show t2 ++ ")"
showTerm (Unop name t) = "(@" ++ name ++ " " ++ show t ++ ")"

var :: String -> Term
var s = Var s

bool :: String -> Term
bool str = if str == "#t"
	then (Boolean True)
	else if str == "#f"
		then (Boolean False)
		else error $ str ++ " is not a boolean value"

syn :: String -> Term
syn s = Syn s

num :: Int -> Term
num n = Num n

ab :: Term -> Term -> Term
ab (Var x) t = Abstr (Var x) t
ab x _ = error ("Term " ++ show x ++ " is not a variable")

ap :: Term -> Term -> Term
ap t1 t2 = Ap t1 t2

fv :: Term -> [Term]
fv (Var x) = [Var x]
fv (Ap t1 t2) = (fv t1) ++ (fv t2)
fv (Abstr x m) = removeAllInst x $ fv m

sub :: Term -> (Term, Term) -> Term
sub (Var x) (Var y, n) = if y == x
	then n
	else (Var x)
sub (Num n) _ = Num n
sub (Boolean b) _ = Boolean b
sub (Ap t1 t2) s = Ap (sub t1 s) (sub t2 s)
sub (Binop name t1 t2) s = Binop name (sub t1 s) (sub t2 s)
sub (Unop name t) s = Unop name (sub t s)
sub a@(Abstr (Var x) t) (Var y, n) = if y == x
	then a
	else Abstr (Var x) $ sub t (Var y, n)

betaReduce :: [(String, Term)] -> Term -> Term
betaReduce s t = betaR $ termSynReplace s t

betaR :: Term -> Term
betaR (Ap (Abstr x t) v) = betaR $ sub t (x, v)
betaR (Ap t1 t2) = betaR (Ap (betaR t1) (betaR t2))
betaR (Binop name t1 t2) = builtinBinopBetaR (Binop name (betaR t1) (betaR t2))
betaR (Unop name t) = builtinUnopBetaR (Unop name (betaR t))
betaR t = t

termSynReplace :: [(String, Term)] -> Term -> Term
termSynReplace _ (Var x) = Var x
termSynReplace _ (Num n) = Num n
termSynReplace _ (Boolean b) = Boolean b
termSynReplace s (Abstr x t) = (Abstr x (termSynReplace s t))
termSynReplace s (Ap t1 t2) = (Ap (termSynReplace s t1) (termSynReplace s t2))
termSynReplace s (Binop name t1 t2) = Binop name (termSynReplace s t1) (termSynReplace s t2)
termSynReplace s (Syn name) = case lookup name s of
	Just t -> t
	Nothing -> error $ name ++ " is not a defined term synonym"

subSyn :: [(String, Term)] -> Term -> Term
subSyn syns (Syn name) = case lookup name syns of
	Just t -> t
	Nothing -> error $ "The name " ++ name ++ " is not currently defined"
subSyn _ t = t

removeAllInst :: (Eq a) => a -> [a] -> [a]
removeAllInst _ [] = []
removeAllInst v (n:ns) = if v == n
	then removeAllInst v ns
	else n:(removeAllInst v ns)

-- Facilities for dealing with built in data types and
-- and their application

builtinBinopBetaR :: Term -> Term
builtinBinopBetaR t@(Binop name (Num _) (Num _)) =
	case lookup name builtinArithmeticBinOps of
		Just reduceFunc -> reduceFunc t
		Nothing -> error $ show t ++ " is not a builtin arithmetic application"
builtinBinopBetaR t@(Binop name (Boolean _) (Boolean _)) =
	case lookup name builtinBooleanBinOps of
		Just reduceFunc -> reduceFunc t
		Nothing -> error $ show t ++ " is not a builtin boolean application"

builtinUnopBetaR :: Term -> Term
builtinUnopBetaR t@(Unop name (Num _)) =
	case lookup name builtinArithmeticUnOps of
		Just reduceFunc -> reduceFunc t
		Nothing -> error $ show t ++ " is not a builtin arithmetic application"
builtinUnopBetaR t@(Unop name (Boolean _)) =
	case lookup name builtinBooleanUnOps of
		Just reduceFunc -> reduceFunc t
		Nothing -> error $ show t ++ " is not a builtin boolean application"

builtinArithmeticBinOps =
	[("+", reduceArithBinop (+))
	,("*", reduceArithBinop (*))
	,("/", reduceArithBinop div)]

builtinArithmeticUnOps =
	[("-", reduceArithUnop (*(-1)))]

reduceArithBinop :: (Int -> Int -> Int) -> Term -> Term
reduceArithBinop op (Binop name (Num n1) (Num n2)) = Num (op n1 n2)
reduceArithBinop op (Binop name t1 t2) =
	reduceArithBinop op (Binop name (betaR t1) (betaR t2))

reduceArithUnop :: (Int -> Int) -> Term -> Term
reduceArithUnop op (Unop name (Num n)) = Num (op n)
reduceArithUnop op (Unop name t) =
	reduceArithUnop op (Unop name (betaR t))

builtinBooleanBinOps =
	[("and", reduceBoolBinop (&&))
	,("or", reduceBoolBinop (||))]

builtinBooleanUnOps =
	[("not", reduceBoolUnop not)]

reduceBoolBinop :: (Bool -> Bool -> Bool) -> Term -> Term
reduceBoolBinop op (Binop name (Boolean b1) (Boolean b2)) = Boolean (op b1 b2)
reduceBoolBinop op (Binop name b1 b2) =
	reduceBoolBinop op (Binop name (betaR b1) (betaR b2))

reduceBoolUnop :: (Bool -> Bool) -> Term -> Term
reduceBoolUnop op (Unop name (Boolean n)) = Boolean (op n)
reduceBoolUnop op (Unop name t) =
	reduceBoolUnop op (Unop name (betaR t))

stdlib =
	arithmeticBinops ++
	arithmeticUnops ++
	booleanBinops ++
	booleanUnops ++
	primitives

arithmeticBinops =
	[("+", (Abstr (Var "x") (Abstr (Var "y") (Binop "+" (Var "x") (Var "y")))))
	,("*", (Abstr (Var "x") (Abstr (Var "y") (Binop "*" (Var "x") (Var "y")))))
	,("/", (Abstr (Var "x") (Abstr (Var "y") (Binop "/" (Var "x") (Var "y")))))]

arithmeticUnops =
	[("-", (Abstr (Var "x") (Unop "-" (Var "x"))))]

booleanBinops =
	[("and", (Abstr (Var "x") (Abstr (Var "y") (Binop "and" (Var "x") (Var "y")))))
	,("or", (Abstr (Var "x") (Abstr (Var "y") (Binop "or" (Var "x") (Var "y")))))]

booleanUnops =
	[("not", (Abstr (Var "x") (Unop "not" (Var "x"))))]

primitives =
	[("I", (Abstr (Var "x") (Var "x")))
	,("K", (Abstr (Var "x") (Abstr (Var "y") (Var "x"))))
	,("S", (Abstr (Var "x") (Abstr (Var "y") (Abstr (Var "z")
		(Ap
			(Ap (Var "x") (Var "z"))
			(Ap (Var "y") (Var "z")))))))]