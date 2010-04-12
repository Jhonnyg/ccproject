import System.Environment (getArgs)
import System.Exit (exitFailure)

import Absjavalette
import Lexjavalette
import Parjavalette
import ErrM

import Printjavalette

import TypeChecker 

import Compiler

check :: String -> IO () 
check s = case pProgram (myLexer s) of
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
																					case compile tree' of
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
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: main <SourceFile>"
                         exitFailure
