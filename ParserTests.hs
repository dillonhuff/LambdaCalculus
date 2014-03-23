module ParserTests() where

import Lambda
import Lexer
import Parser
import Test.HUnit

allParserTests = runTestTT tests

tests = TestList
	[parseExpr_oneVar
	,parseExpr_oneVarParens
	,parseExpr_oneAbstr
	,parseExpr_nestedAbstr
	,parseExpr_noParenAppl
	,parseExpr_3VarAppl
	,parseExpr_3VarParenAppl
	,parseExpr_4VarAppl
	,parseExpr_nestedAppl]

parseExpr_oneVar = 
	parseTermTest "x" (var "x")

parseExpr_oneVarParens =
	parseTermTest "(x)" (var "x")

parseExpr_oneAbstr =
	parseTermTest "\\x. K" (ab (var "x") (syn "K"))

parseExpr_nestedAbstr =
	parseTermTest "\\x. \\y.x" (ab (var "x") (ab (var "y") (var "x")))

parseExpr_noParenAppl =
	parseTermTest "x y" (ap (var "x") (var "y"))

parseExpr_3VarAppl =
	parseTermTest "x y z" (ap (ap (var "x") (var "y")) (var "z"))

parseExpr_4VarAppl =
	parseTermTest "a b c d" (ap (ap (ap (var "a") (var "b")) (var "c")) (var "d"))

parseExpr_nestedAppl =
	parseTermTest "a (\\x. K) (a c)"
		(ap (ap (var "a") (ab (var "x") (syn "K"))) (ap (var "a") (var "c")))

parseExpr_3VarParenAppl =
	parseTermTest "(x y) z" (ap (ap (var "x") (var "y")) (var "z"))

parseTermTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(parseTerm $ programToks input))