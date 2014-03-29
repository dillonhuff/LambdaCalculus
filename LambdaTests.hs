module LambdaTests() where

import ErrorHandling
import Lambda
import Test.HUnit

allLambdaTests = runTestTT tests

tests = TestList
	[inferType_variable
	,inferType_number
	,inferType_boolean]

inferType_variable =
	inferTypeTest (var "x") ANY

inferType_number =
	inferTypeTest (num 12) INT

inferType_boolean =
	inferTypeTest (bool "#t") BOOL

inferTypeTest input expected = TestCase
 (assertEqual ("Input: " ++ show input)
 	expected
 	(extractValue $ inferType input))