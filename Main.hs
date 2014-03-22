module Main(main) where

import System.IO
import Lexer
import Parser

main = do
	putStrLn "Enter file name:"
	name <- getLine
	handle <- openFile name ReadMode
	contents <- hGetContents handle
	let parsedProgram = parseToks $ programToks contents
	putStrLn $ show parsedProgram