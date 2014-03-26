module Lambda(
	Term, isSyn, isVar, isNum, var, syn, num, ab, ap,
	betaReduce,
	stdlib) where

data Term
	= Var String
	| Num Int
	| Abstr Term Term
	| Ap Term Term
	| Syn String
	| BAp String Term Term
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

showTerm :: Term -> String
showTerm (Var name) = name
showTerm (Num n) = show n
showTerm (Abstr (Var x) t) = "(\\" ++ x ++ ". " ++ show t ++ ")"
showTerm (Ap t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
showTerm (Syn name) = name
showTerm (BAp name t1 t2) = "(@" ++ name ++ " " ++ show t1 ++ " " ++ show t2 ++ ")"

var :: String -> Term
var s = Var s

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
sub (Ap t1 t2) s = Ap (sub t1 s) (sub t2 s)
sub (BAp name t1 t2) s = BAp name (sub t1 s) (sub t2 s)
sub a@(Abstr (Var x) t) (Var y, n) = if y == x
	then a
	else Abstr (Var x) $ sub t (Var y, n)

betaReduce :: [(String, Term)] -> Term -> Term
betaReduce s t = betaR $ termSynReplace s t

betaR :: Term -> Term
betaR (Ap (Abstr x t) v) = betaR $ sub t (x, v)
betaR (Ap t1 t2) = betaR (Ap (betaR t1) (betaR t2))
betaR (BAp name t1 t2) = builtinBetaR (BAp name t1 t2)
betaR t = t

termSynReplace :: [(String, Term)] -> Term -> Term
termSynReplace _ (Var x) = Var x
termSynReplace _ (Num n) = Num n
termSynReplace s (Abstr x t) = (Abstr x (termSynReplace s t))
termSynReplace s (Ap t1 t2) = (Ap (termSynReplace s t1) (termSynReplace s t2))
termSynReplace s (BAp name t1 t2) = BAp name (termSynReplace s t1) (termSynReplace s t2)
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

builtinBetaR :: Term -> Term
builtinBetaR t@(BAp name t1 t2) = case lookup name builtinAps of
	Just reduceFunc -> reduceFunc t
	Nothing -> error $ show t ++ " is not a builtin application"

builtinAps =
	[("+", reduceArithBinop (+))
	,("-", reduceArithBinop (-))
	,("*", reduceArithBinop (*))
	,("/", reduceArithBinop div)]

reduceArithBinop :: (Int -> Int -> Int) -> Term -> Term
reduceArithBinop op (BAp name (Num n1) (Num n2)) = Num (op n1 n2)
reduceArithBinop op (BAp name t1 t2) = reduceArithBinop op (BAp name (betaR t1) (betaR t2))

stdlib = arithmetic ++ primitives

arithmetic =
	[("+", (Abstr (Var "x") (Abstr (Var "y") (BAp "+" (Var "x") (Var "y")))))
	,("-", (Abstr (Var "x") (Abstr (Var "y") (BAp "-" (Var "x") (Var "y")))))
	,("*", (Abstr (Var "x") (Abstr (Var "y") (BAp "*" (Var "x") (Var "y")))))
	,("/", (Abstr (Var "x") (Abstr (Var "y") (BAp "/" (Var "x") (Var "y")))))]

primitives =
	[("I", (Abstr (Var "x") (Var "x")))
	,("K", (Abstr (Var "x") (Abstr (Var "y") (Var "x"))))
	,("S", (Abstr (Var "x") (Abstr (Var "y") (Abstr (Var "z")
		(Ap
			(Ap (Var "x") (Var "z"))
			(Ap (Var "y") (Var "z")))))))]