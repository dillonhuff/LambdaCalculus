module REPL(startREPL) where

import ErrorHandling
import Lambda
import Lexer
import Parser
import System.IO
import Text.ParserCombinators.Parsec

startREPL = do
	putStrLn welcomeMessage
	doCommand stdlib

doCommand replState = do
	putStr ">>> "
	command <- getLine
	-- Check if input was meta-command or lambda calculus
	if head command == ':'
		then doMetaCommand replState command
		else lambdaCalcCommand replState command

doMetaCommand state command = case command of
	":load" -> loadFile state
	":state"-> showState state
	":quit" -> quit
	_ -> reportError state (Repl (command ++ " is not a valid meta-command"))

reportError state error = do
	putStrLn $ show error
	doCommand state

loadFile state = do
	putStr "Enter name of file to load: "
	fileName <- getLine
	fHandle <- openFile fileName ReadMode
	lamdaProg <- hGetContents fHandle
	putStrLn $ "File Contents " ++ lamdaProg
	hClose fHandle
	case parseProgram lamdaProg of
		Left err -> reportError state err
		Right newDefs -> doCommand (state ++ (map termSynToPair newDefs))

showState state = do
	putStrLn $ "Current state is:\n" ++ (show state)
	doCommand state

quit = do
	putStrLn "Goodbye!"

lambdaCalcCommand replState command = do
	parseReplInput replState command

parseReplInput state toks = case parseTerm toks of
	Left err -> reportError state err
	Right term -> showOutput state (show $ betaReduce state term)

showOutput state out = do
	putStrLn out
	doCommand state

installDef :: [(String, Term)] -> [PosTok] -> ThrowsError [(String, Term)]
installDef state toks = case parseTermSynDef toks of
	Just def -> Right ((termSynToPair def):state)
	Nothing -> Left (Repl (show toks ++ " Invalid input"))

welcomeMessage = "Hello and welcome to Dillon Huff's lambda calculus interpreter!"