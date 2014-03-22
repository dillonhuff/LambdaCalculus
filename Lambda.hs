module Lambda(Term, isVar, var, ab, ap) where

data Term
	= Var String
	| Abstr Term Term
	| Ap Term Term
	deriving (Eq)

instance Show Term where
	show = showTerm

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

showTerm :: Term -> String
showTerm (Var name) = name
showTerm (Abstr (Var x) t) = "(\\" ++ x ++ ". " ++ show t ++ ")"
showTerm (Ap t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"

var :: String -> Term
var s = Var s

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
sub (Ap t1 t2) s = Ap (sub t1 s) (sub t2 s)
sub a@(Abstr (Var x) t) (Var y, n) = if y == x
	then a
	else Abstr (Var x) $ sub t (Var y, n)

betaR :: Term -> Term -> Term
betaR (Abstr x t) v = sub t (x, v)
betaR u v = error $ show u ++ " is not a lambda abstraction"

removeAllInst :: (Eq a) => a -> [a] -> [a]
removeAllInst _ [] = []
removeAllInst v (n:ns) = if v == n
	then removeAllInst v ns
	else n:(removeAllInst v ns)