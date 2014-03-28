module Parser(
	parseProgram,
	parseTerm,
	parseTermSynDef,
	TermSyn,
	termSynToPair) where

import ErrorHandling
import Lambda
import Lexer
import Text.Parsec

data TermSyn = TS String Term
	deriving (Eq)

termSynToPair :: TermSyn -> (String, Term)
termSynToPair (TS name t) = (name, t)

instance Show TermSyn where
	show = showTermSyn

showTermSyn :: TermSyn -> String
showTermSyn (TS name t) = name ++ " = " ++ show t

parseProgram :: String -> ThrowsError [TermSyn]
parseProgram text = case programToks text of
	Left lexErr -> Left lexErr
	Right programTokens -> case parseToks programTokens of
		Left parseErr -> Left parseErr
		Right termSyns -> Right termSyns

parseToks :: [PosTok] -> ThrowsError [TermSyn]
parseToks ts = case parse pLambdaProg "Lambda Calc" ts of
	Left err -> Left $ Parse err
	Right defs -> Right defs

parseTermSynDef :: [PosTok] -> Maybe TermSyn
parseTermSynDef ts = case parse pTermSynDef "Lambda Calc" ts of
	Left err -> Nothing
	Right tDef -> Just tDef

parseTerm :: String -> ThrowsError Term
parseTerm ts = case programToks ts of
	Left err -> Left err
	Right t -> case parse pTerm "Lambda Calc" t of
		Left err -> Left $ Parse err
		Right term -> Right term

pLambdaProg = do
	termSyns <- many pTermSynDef
	return termSyns

pTermSynDef = do
	lTok DEF
	name <- termSyn
	lTok ASSIGN
	body <- pTerm
	lTok SEMICOLON
	return $ TS (termName $ tok name) body

pTerm = do
	t <- pMultiTerm
	return t

pSTerm = do
	st <- pParenTerm
		<|> pVar
		<|> pNum
		<|> pBool
		<|> pIfThenElse
		<|> pTypeSynonym
		<|> pAbstr
	return st

pParenTerm = do
	lTok LPAREN
	t <- pTerm
	lTok RPAREN
	return t

pVar = do
	v <- varTok
	return $ getTerm $ tok v

pNum = do
	n <- numTok
	return $ getTerm $ tok n

pBool = do
	val <- boolTok
	return $ getTerm $ tok val

pIfThenElse = do
	lTok IF
	condition <- pTerm
	lTok THEN
	thenTerm <- pTerm
	lTok ELSE
	elseTerm <- pTerm
	return $ ifThenElse condition thenTerm elseTerm

pTypeSynonym = do
	s <- termSyn
	return $ getTerm $ tok s

pAbstr = do
	lTok LAMBDA
	v <- pVar
	lTok DOT
	t <- pTerm
	return $ ab v t

pMultiTerm = do
	terms <- many1 pSTerm
	return $ multiTerm terms

multiTerm :: [Term] -> Term
multiTerm [] = error $ "Term list is empty"
multiTerm [t] = t
multiTerm (t:n:ts) = foldl  ap (ap t n) ts

termSyn :: (Monad m) => ParsecT [PosTok] u m PosTok
termSyn = tokenPrim show updatePos termSynTok
	where
		termSynTok pt = if isTermSyn (tok pt) then Just pt else Nothing

varTok :: (Monad m) => ParsecT [PosTok] u m PosTok
varTok = tokenPrim show updatePos varTok
	where
		varTok pt = if isVarTok (tok pt) then Just pt else Nothing

numTok :: (Monad m) => ParsecT [PosTok] u m PosTok
numTok = tokenPrim show updatePos numTok
	where
		numTok pt = if isNumTok (tok pt) then Just pt else Nothing

boolTok :: (Monad m) => ParsecT [PosTok] u m PosTok
boolTok = tokenPrim show updatePos boolTok
	where
		boolTok pt = if isBoolTok (tok pt) then Just pt else Nothing

lTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
lTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos pos _ [] = pos
