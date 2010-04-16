import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.IO
import System

import Absjavalette
import Lexjavalette
import Parjavalette
import ErrM

import Printjavalette

import TypeChecker 

import Compiler

import List (intersperse)

check :: String -> String -> IO () 
check n s = case pProgram (myLexer s) of
            Bad err  -> do
														hPutStr stderr "ERROR"
														putStrLn "SYNTAX ERROR"
														putStrLn err
														exitFailure 
            Ok  tree -> case typecheck tree of
                          Bad err -> do
																				hPutStr stderr "ERROR"
																				putStrLn "TYPECHECK ERROR"
																				putStrLn err
																				exitFailure 
                          Ok tree' -> do
																					let class_name = takeBaseName n
																					case compile class_name tree' of
																						Bad err -> do
																													hPutStr stderr "ERROR"
																													putStrLn "COMPILE ERROR"
																													putStrLn err
																													exitFailure
																						Ok prg    -> do
																														--putStrLn "Compile: OK"
																														--mapM_ (putStrLn) prg
																														let jasmine_code = concat $ intersperse "\n" prg
																														
																														-- write jasmin code
																														writeFile (genJOutputDir n) jasmine_code
																														
																														-- run jasmin on output file
																														system $ "java -jar lib/jasmin.jar" ++ (genJOutputFlag n) ++ (genJOutputDir n)
																														
																														-- ALRIGHT!
																														hPutStr stderr "OK"
																														exitSuccess

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
		readFile file >>= check file
            _      -> do putStrLn "Usage: jlc <SourceFile.jl>"
                         exitFailure

genJOutputDir :: String -> String
genJOutputDir n = case (takeDirectory n) of
	"" -> (takeBaseName n) ++ ".j"
	otherwise -> ((takeDirectory n) ++ "/" ++ (takeBaseName n) ++ ".j")

genJOutputFlag :: String -> String
genJOutputFlag n = case (takeDirectory n) of
	"" -> " "
	otherwise -> " -d " ++ ((takeDirectory n) ++ "/ ")