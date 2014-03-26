module Lambda(
	Term, isSyn, isVar, isNum, var, syn, num, ab, ap,
	betaReduce,
	arithmetic) where

data Term
	= Var String
	| Num Int
	| Abstr Term Term
	| Ap Term Term
	| Syn String
	| Plus Term Term
	| Minus Term Term
	| Times Term Term
	| Divide Term Term
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
showTerm (Plus t1 t2) = "(@+ " ++ show t1 ++ " " ++ show t2 ++ ")"
showTerm (Minus t1 t2) = "(@- " ++ show t1 ++ " " ++ show t2 ++ ")"
showTerm (Times t1 t2) = "(@* " ++ show t1 ++ " " ++ show t2 ++ ")"
showTerm (Divide t1 t2) = "(@/ " ++ show t1 ++ " " ++ show t2 ++ ")"

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
sub (Plus t1 t2) s = Plus (sub t1 s) (sub t2 s)
sub (Minus t1 t2) s = Minus (sub t1 s) (sub t2 s)
sub (Times t1 t2) s = Times (sub t1 s) (sub t2 s)
sub (Divide t1 t2) s = Divide (sub t1 s) (sub t2 s)
sub a@(Abstr (Var x) t) (Var y, n) = if y == x
	then a
	else Abstr (Var x) $ sub t (Var y, n)

betaReduce :: [(String, Term)] -> Term -> Term
betaReduce s t = betaR $ termSynReplace s t

betaR :: Term -> Term
betaR (Ap (Abstr x t) v) = betaR $ sub t (x, v)
betaR (Ap t1 t2) = betaR (Ap (betaR t1) (betaR t2))
betaR (Plus (Num n1) (Num n2)) = Num (n1 + n2)
betaR (Minus (Num n1) (Num n2)) = Num (n1 - n2)
betaR (Times (Num n1) (Num n2)) = Num (n1 * n2)
betaR (Divide (Num n1) (Num n2)) = Num (div n1 n2)
betaR (Plus t1 t2) = betaR (Plus (betaR t1) (betaR t2)) -- DANGER! This can lead to infinite loops with bad input!
betaR (Minus t1 t2) = betaR (Minus (betaR t1) (betaR t2))
betaR (Times t1 t2) = betaR (Times (betaR t1) (betaR t2))
betaR (Divide t1 t2) = betaR (Divide (betaR t1) (betaR t2))
betaR t = t

termSynReplace :: [(String, Term)] -> Term -> Term
termSynReplace _ (Var x) = Var x
termSynReplace _ (Num n) = Num n
termSynReplace s (Abstr x t) = (Abstr x (termSynReplace s t))
termSynReplace s (Ap t1 t2) = (Ap (termSynReplace s t1) (termSynReplace s t2))
termSynReplace s (Plus t1 t2) = (Plus (termSynReplace s t1) (termSynReplace s t2))
termSynReplace s (Minus t1 t2) = (Minus (termSynReplace s t1) (termSynReplace s t2))
termSynReplace s (Times t1 t2) = (Times (termSynReplace s t1) (termSynReplace s t2))
termSynReplace s (Divide t1 t2) = (Divide (termSynReplace s t1) (termSynReplace s t2))
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

-- Builtin definitions for libraries
arithmetic =
	[("+", (Abstr (Var "x") (Abstr (Var "y") (Plus (Var "x") (Var "y")))))
	,("-", (Abstr (Var "x") (Abstr (Var "y") (Minus (Var "x") (Var "y")))))
	,("*", (Abstr (Var "x") (Abstr (Var "y") (Times (Var "x") (Var "y")))))
	,("/", (Abstr (Var "x") (Abstr (Var "y") (Divide (Var "x") (Var "y")))))]