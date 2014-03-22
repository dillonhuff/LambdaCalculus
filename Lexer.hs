module Lexer() where

import Lambda
import Text.ParserCombinators.Parsec

data Tok = ASSIGN | DOT | LAMBDA | LPAREN | RPAREN | T Term | S String

resToOps = [("=", ASSIGN), (".", DOT), ("\\", LAMBDA), ("(", LPAREN), (")", RPAREN)]

programToks :: String -> [Tok]
programToks progText = case parse pToks "Lambda Calc" progText of
	Left err -> error $ show err
	Right ts -> ts

pToks = do
	spaces
	ts <- sepBy pTok spaces
	return ts

pTok = do
	t <- pVar
		<|> pTermSyn
		<|> pReserved
	return t

pVar = do
	startChar <- lower
	rest <- many alphaNum
	return $ T $ var (startChar:rest)

pTermSyn = do
	startChar <- upper
	rest <- many alphaNum
	return $ S (startChar:rest)

pReserved = do
	r <- string "="
		<|> string "\\"
		<|> string "."
		<|> string "("
		<|> string ")"
	return $ case lookup r resToOps of
		Just op -> op
		Nothing -> error $ show r ++ " is not a valid input"
