module Lambda(
	Term, isSyn, isVar, var, syn, ab, ap,
	betaReduce, reduce) where

data Term
	= Var String
	| Abstr Term Term
	| Ap Term Term
	| Syn String
	deriving (Eq)

instance Show Term where
	show = showTerm

isSyn :: Term -> Bool
isSyn (Syn _) = True
isSyn _ = False

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

showTerm :: Term -> String
showTerm (Var name) = name
showTerm (Abstr (Var x) t) = "(\\" ++ x ++ ". " ++ show t ++ ")"
showTerm (Ap t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
showTerm (Syn name) = name

var :: String -> Term
var s = Var s

syn :: String -> Term
syn s = Syn s

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

betaReduce :: [(String, Term)] -> Term -> Term
betaReduce s t = betaR $ termSynReplace s t

betaR :: Term -> Term
betaR (Ap (Abstr x t) v) = sub t (x, v)
betaR (Ap t1 t2) = betaR (Ap (betaR t1) t2)
betaR t = t

reduce :: Term -> Term
reduce (Ap (Abstr x t) v) = reduce $ sub t (x, v)
reduce t = t

termSynReplace :: [(String, Term)] -> Term -> Term
termSynReplace _ (Var x) = Var x
termSynReplace s (Abstr x t) = (Abstr x (termSynReplace s t))
termSynReplace s (Ap t1 t2) = (Ap (termSynReplace s t1) (termSynReplace s t2))
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