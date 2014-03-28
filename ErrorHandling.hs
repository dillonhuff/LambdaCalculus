module ErrorHandling(
	LCError(Parse, Repl, Default),
	extractValue) where

import Control.Monad.Error
import Text.Parsec.Error

type ThrowsError = Either LCError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data LCError
	= Parse ParseError
	| Repl String
	| Default String

showError :: LCError -> String
showError (Parse parseError) = "Parse error at " ++ show parseError
showError (Repl replError) = "REPL: " ++ replError
showError (Default str) = str

instance Show LCError where
	show = showError

instance Error LCError where
	noMsg = Default "An error has occured"
	strMsg = Default