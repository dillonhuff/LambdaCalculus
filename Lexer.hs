module Lexer(
	programToks, isVarTok, isNumTok, isBoolTok,
	PosTok, pos, tok, getTerm, isTermSyn, termName,
	Tok(ASSIGN, DOT, LAMBDA, LPAREN, RPAREN, SEMICOLON, IF, THEN, ELSE, DEF, T)) where

import Lambda
import Text.ParserCombinators.Parsec

data PosTok = PT Tok SourcePos
	
instance Show PosTok where
	show = showPT

showPT :: PosTok -> String
showPT (PT t _) = show t

tok :: PosTok -> Tok
tok (PT t _) = t

pos :: PosTok -> SourcePos
pos (PT _ p) = p

instance Eq PosTok where
	(==) = ptEq

ptEq (PT t1 _) (PT t2 _) = t1 == t2

data Tok = ASSIGN | DOT | LAMBDA | LPAREN | RPAREN  | SEMICOLON |
	IF | THEN | ELSE | DEF | T Term
	deriving (Eq, Show)

isVarTok :: Tok -> Bool
isVarTok (T t) = isVar t
isVarTok _ = False

isNumTok :: Tok -> Bool
isNumTok (T t) = isNum t
isNumTok _ = False

isBoolTok :: Tok -> Bool
isBoolTok (T t) = isBool t
isBoolTok _ = False

getTerm :: Tok -> Term
getTerm (T t) = t
getTerm x = error $ show x ++ " is not a term"

isTermSyn :: Tok -> Bool
isTermSyn (T t) = isSyn t
isTermSyn _ = False

termName :: Tok -> String
termName (T t) = if isSyn t
	then show t
	else error $ show t ++ " is not a term synonym"

resToOps =
	[("=", ASSIGN), (".", DOT), ("\\", LAMBDA), ("(", LPAREN)
	,(")", RPAREN), (";", SEMICOLON), ("if", IF), ("then", THEN)
	,("else", ELSE), ("def", DEF)]

programToks :: String -> [PosTok]
programToks progText = case parse pToks "Lambda Calc" progText of
	Left err -> error $ show err
	Right ts -> ts

pToks = do
	spaces
	ts <- sepBy pTok spaces
	return ts

pTok = do
	t <- try pReserved
		<|> pVar
		<|> pNum
		<|> pBool
		<|> pTermSyn
	return t

pVar = do
	p <- getPosition
	startChar <- lower
	rest <- many alphaNum
	return $ PT (T $ var (startChar:rest)) p

pTermSyn = do
	p <- getPosition
	startChar <- upper
	rest <- many alphaNum
	return $ PT (T $ syn (startChar:rest)) p

pNum = do
	p <- getPosition
	digs <- many1 digit
	return $ PT (T $ num (read digs)) p

pBool = do
	p <- getPosition
	value <- try (string "#t")
		<|> (string "#f")
	return $ PT (T $ bool value) p

pReserved = do
	p <- getPosition
	r <- string "="
		<|> string "\\"
		<|> string "."
		<|> string "("
		<|> string ")"
		<|> string ";"
		<|> string "+"
		<|> string "-"
		<|> string "*"
		<|> string "/"
		<|> string "and"
		<|> string "or"
		<|> string "not"
		<|> string "if"
		<|> string "then"
		<|> string "else"
		<|> string "def"
	return $ case lookup r resToOps of
		Just op -> PT op p
		Nothing -> PT (T $ syn r) p
