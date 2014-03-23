module Parser(
	parseToks,
	parseTerm,
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
	return $ TS (termName $ tok name) body

pTerm = do
	t <- pVar
		<|> pTypeSynonym
	return t

pVar = do
	v <- varTok
	return $ getTerm $ tok v

pTypeSynonym = do
	s <- termSyn
	return $ getTerm $ tok s

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
