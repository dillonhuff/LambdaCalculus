module REPL(startREPL) where

import Lambda
import Lexer
import Parser
import System.IO
import Text.ParserCombinators.Parsec

startREPL = do
	putStrLn welcomeMessage
	doCommand []

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
	_ -> putStrLn $ show command ++ " is not a valid meta-command"

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
	let result = parseTerm $ programToks command
	putStrLn $ show $ termBetaReduce replState result
	doCommand replState

welcomeMessage = "Hello and welcome to Dillon Huff's lambda calculus interpreter!"