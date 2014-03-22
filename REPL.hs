module REPL(startREPL) where

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

doMetaCommand state command = case parsedCommand of
	"load" -> loadFile state ""
	"quit" -> quit
	_ -> putStrLn $ show command ++ " is not a valid meta-command"

loadFile state fileName = do
	putStr "I actually do nothing"

quit = do
	putStr "Goodbye!"

lambdaCalcCommand replState command = doCommand replState

welcomeMessage = "Hello and welcome to Dillon Huff's lambda calculus interpreter!"