module Parser(
	parseToks,
	parseTerm,
	TermSyn,
	termSynToPair) where

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

parseToks :: [PosTok] -> [TermSyn]
parseToks ts = case parse pLambdaProg "Lambda Calc" ts of
	Left err -> error $ show err
	Right defs -> defs

parseTerm :: [PosTok] -> Term
parseTerm ts = case parse pTerm "Lambda Calc" ts of
	Left err -> error $ show err
	Right t -> t

pLambdaProg = do
	termSyns <- many pTermSynDef
	return termSyns

pTermSynDef = do
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

lTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
lTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos pos _ [] = pos
