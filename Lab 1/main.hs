import System.Environment (getArgs)
import System.Exit (exitFailure)

import Absjavalette
import Lexjavalette
import Parjavalette
import ErrM

import Printjavalette

import TypeChecker 

import Compiler

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
																					case compile n tree' of
																						Bad err -> do
																													putStrLn "COMPILE ERROR"
																													putStrLn err
																													exitFailure
																						Ok prg    -> do
																														--putStrLn "Compile: OK"
																														mapM_ (putStrLn) prg

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
		readFile file >>= check (getfilename file)
		--putStrLn (getfilename file)
            _      -> do putStrLn "Usage: main <SourceFile>"
                         exitFailure

getfilename :: String -> String
getfilename n = reverse $ takeWhile (\e -> e /= '/') $ reverse (takeWhile (\e -> e /= '.') n)
