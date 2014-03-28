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
	_ -> reportBadMetaCommand state command

reportBadMetaCommand state command = do
	putStrLn $ show (Repl (command ++ " is not a valid meta-command"))
	doCommand state

loadFile state = do
	putStr "Enter name of file to load: "
	fileName <- getLine
	fHandle <- openFile fileName ReadMode
	lamdaProg <- hGetContents fHandle
	putStrLn $ "File Contents " ++ lamdaProg
	let newDefs = parseToks $ programToks lamdaProg
	hClose fHandle
	doCommand (state ++ (map termSynToPair newDefs))

showState state = do
	putStrLn $ "Current state is:\n" ++ (show state)
	doCommand state

quit = do
	putStrLn "Goodbye!"

lambdaCalcCommand replState command = do
	parseReplInput replState $ programToks command

parseReplInput state toks = case parseTerm toks of
	Just expr -> showOutput state (show $ betaReduce state expr)
	Nothing -> doCommand (installDef state toks)

showOutput state out = do
	putStrLn out
	doCommand state

installDef :: [(String, Term)] -> [PosTok] -> [(String, Term)]
installDef state toks = (termSynToPair newDef):state
	where
		newDef = case parseTermSynDef toks of
			Just def -> def
			Nothing -> error $ show toks ++ " Invalid input"

welcomeMessage = "Hello and welcome to Dillon Huff's lambda calculus interpreter!"