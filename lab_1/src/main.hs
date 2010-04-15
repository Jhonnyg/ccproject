import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
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
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> case typecheck tree of
                          Bad err -> do putStrLn "TYPE ERROR"
                                        putStrLn err
                                        exitFailure 
                          Ok tree' -> do
																					--putStrLn "Typecheck: OK!"
																					--fail $ (show tree')
																					let class_name = takeBaseName n
																					case compile class_name tree' of
																						Bad err -> do
																													putStrLn "COMPILE ERROR"
																													putStrLn err
																													exitFailure
																						Ok prg    -> do
																														--putStrLn "Compile: OK"
																														--mapM_ (putStrLn) prg
																														let jasmine_code = concat $ intersperse "\n" prg
																														--putStrLn ((takeDirectory n) ++ "/" ++ class_name ++ ".j")
																														
																														-- write jasmin code
																														let output_file = ((takeDirectory n) ++ "/" ++ class_name ++ ".j")
																														writeFile output_file jasmine_code
																														
																														-- run jasmin on output file
																														let jasmin_flags = " -d " ++ ((takeDirectory n) ++ "/ ")
																														putStrLn $ "java -jar lib/jasmin.jar" ++ jasmin_flags ++ output_file
																														system $ "java -jar lib/jasmin.jar" ++ jasmin_flags ++ output_file
																														exitSuccess

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
		readFile file >>= check file
		--putStrLn (getfilename file)
            _      -> do putStrLn "Usage: main <SourceFile>"
                         exitFailure

