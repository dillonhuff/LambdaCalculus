module Parser(
	parseToks,
	TermSyn) where

import Lambda
import Lexer
import Text.Parsec

data TermSyn = TS String Term
	deriving (Eq)

instance Show TermSyn where
	show = showTermSyn

showTermSyn :: TermSyn -> String
showTermSyn (TS name t) = name ++ " = " ++ show t

parseToks :: [PosTok] -> [TermSyn]
parseToks ts = case parse lambdaProg "Lambda Calc" ts of
	Left err -> error $ show err
	Right defs -> defs

lambdaProg = do
	termSyns <- many termSynDef
	return termSyns

termSynDef = do
	name <- termSyn
	lTok ASSIGN
	body <- term
	return $ TS (termName $ tok name) body

term = do
	t <- pVar
		<|> pAbstraction
		<|> pApplication
	return t

pVar = do
	v <- varTok
	return $ getTerm $ tok v

parenTerm = do
	lTok LPAREN
	t <- term
	lTok RPAREN
	return t

pAbstraction = do
	abstr <- pParenAbstr
		<|> pAbstr
	return abstr

pParenAbstr = do
	lTok LPAREN
	abstr <- pAbstr
	lTok RPAREN
	return abstr

pAbstr = do
	lTok LAMBDA
	x <- pVar
	lTok DOT
	t <- term
	return $ ab x t

pApplication = do
	app <- pParenAppl
		<|> pAppl
	return app

pParenAppl = do
	lTok LPAREN
	app <- pAppl
	lTok RPAREN
	return app

pAppl = do
	t1 <- term
	t2 <- term
	return $ ap t1 t2

termSyn :: (Monad m) => ParsecT [PosTok] u m PosTok
termSyn = tokenPrim show updatePos termSynTok
	where
		termSynTok pt = if isTermSyn (tok pt) then Just pt else Nothing

varTok :: (Monad m) => ParsecT [PosTok] u m PosTok
varTok = tokenPrim show updatePos varTok
	where
		varTok pt = if isVarTok (tok pt) then Just pt else Nothing

lTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
lTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos pos _ [] = pos
