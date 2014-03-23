module ParserTests() where

import Lexer
import Parser
import Test.HUnit

allParserTests = runTestTT tests

tests = TestList
	[]

parseTermTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(parseTerm $ programToks input))