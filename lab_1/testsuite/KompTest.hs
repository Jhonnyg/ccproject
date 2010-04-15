-- {-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
-- GHC needs -threaded
module KompTest where

import Control.Monad
import Data.List
import Data.Maybe
import System
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.FilePath
import Data.Char
import RunCommand

data Backend = Backend {
			name :: String,
			objFile :: FilePath -> FilePath,
			run  :: String -> FilePath -> FilePath -> FilePath -> IO Bool
		       }

--
-- * Error reporting and output checking
--

reportErrorColor :: Color 
                 -> String -- ^ command that failed
	         -> String -- ^ how it failed
	         -> FilePath -- ^ source file
	         -> String -- ^ given input
	         -> String -- ^ stdout output
	         -> String -- ^ stderr output
	         -> IO ()
reportErrorColor col c m f i o e =
    do
    putStrLn $ color col $ c ++ " failed: " ++ m
    putStrLn $ "For source file " ++ f ++ ":"
    -- prFile f
    when (not (null i)) $ do
			  putStrLn "Given this input:"
			  putStrLn $ color blue $ i
    when (not (null o)) $ do
			  putStrLn "It printed this to standard output:"
			  putStrLn $ color blue $ o
    when (not (null e)) $ do
			  putStrLn "It printed this to standard error:"
			  putStrLn $ color blue $ e

reportError :: String -- ^ command that failed
	    -> String -- ^ how it failed
	    -> FilePath -- ^ source file
	    -> String -- ^ given input
	    -> String -- ^ stdout output
	    -> String -- ^ stderr output
	    -> IO ()
reportError = reportErrorColor red

data ErrorReport = ErrorReport {
   repErr :: String,
   repSeverity :: Severity,
   repCmd :: String,
   repStdIn :: String,
   repStdOut :: String,
   repStdErr :: String
                 }

data Severity = SWarning | SError | SInfo

defRep = ErrorReport {
   repErr = "OK",
   repSeverity = SInfo,
   repCmd = "",
   repStdIn = "",
   repStdOut = "",
   repStdErr = "" 
 }

rep ?! msg = rep { repErr = msg, repSeverity = SError }
rep ?? msg = rep { repErr = msg, repSeverity = SWarning }

report0 er = case repSeverity er of
                SInfo -> return True
                _ -> do reportError (repCmd er) (repErr er) (repCmd er) (repStdIn er) (repStdOut er) (repStdErr er)
                        return False

report1 Nothing = return True
report1 (Just rep) = report0 rep >> return False


prFile :: FilePath -> IO ()
prFile f = do
	   putStrLn $ "---------------- begin " ++ f ++ " ------------------" 
	   s <- readFile f
	   putStrLn $ color green s
	   putStrLn $ "----------------- end " ++ f ++ " -------------------" 


-- | Report how many tests passed.
report :: String -> [Bool] -> IO ()
report n rs = 
  do let (p,t) = (length (filter id rs), length rs)
     putStrLn $ n ++ ": passed " ++ show p ++ " of " ++ show t ++ " tests"
--
-- * Generic running
--

runProg :: String -- ^ command
	-> FilePath -- ^ source file (for error reporting)
	-> FilePath -- ^ known input file
	-> FilePath -- ^ known output file
	-> IO Bool
runProg c f i o =
    do
    fe <- doesFileExist i
    input <- if fe then readFile i else return ""
    output <- readFile o
    (out,err,s) <- runCommandStrWait c input
    case s of
	   ExitFailure x -> do
			    reportError c ("with status " ++ show x) f input out err
			    return False
	   ExitSuccess -> 
	       do
	       if not (null err) then
		  do
		  reportError c "Printed something to standard error" f input out err
		  return False
	        else if output /= out then
		     do
		     putStrLn $ color red $ c ++ " produced the wrong output:"
		     putStrLn $ "For source file " ++ f ++ ":"
		     prFile f
		     when (not (null input)) $ do
					       putStrLn "Given this input:"
					       putStrLn $ color blue $ input
		     putStrLn "It printed this to standard output:"
		     putStrLn $ color blue $ out
		     putStrLn "It should have printed this:"
		     putStrLn $ color blue $ output
		     return False
		 else do
		      putStrLn "output ok"
		      return True

testProg :: FilePath -- ^ executable
         -> Backend
	 -> FilePath -- ^ test file
	 -> IO Bool
testProg cmd b f = do
    putStr $ "Testing " ++ takeFileName f ++ ": "
    hFlush stdout
    let n = dropExtension f
	o = n ++ ".output"
    let c = objFile b f
    ofe <- doesFileExist o
    if ofe then run b c f (n ++ ".input") o
       else do
	    putStrLn $ color blue $ "skipping: " ++ o ++ " not found"
	    return True

test :: FilePath -> [FilePath] -> Backend -> IO [Bool]
test c fs b =
    do putStrLn $ color green $ "Backend: " ++ name b 
       mapM (testProg c b) fs

--
-- * Compilation
--

testCompilation :: FilePath -> Bool -> [FilePath] -> IO [Bool]
testCompilation c good fs = 
  do x <- doesFileExist c
     forM fs $ \t -> report0 =<< do
       if x then testCompilationProg c good t
            else return $ defRep {repCmd = c} ?! ("compiler " ++ c ++ " not found")

testCompilationProg :: FilePath -> Bool -> FilePath -> IO ErrorReport
testCompilationProg path good f =
    do let c = path ++ " " ++ f
       putStrLn $ takeFileName path ++ " " ++ takeFileName f ++ "..."
       (out,err,_) <- runCommandStrWait c ""
       let rep = defRep {repCmd = f, repStdOut = out, repStdErr = err}
       lns <- return $ lines err
       return $ case filter (not . null) lns of 
           msgs | isOk    msgs -> if good then rep                        else rep ?! "passed BAD program" 
                | isError msgs -> if good then rep ?! "failed OK program" else rep
           _ -> rep ?! "invalid output"
    where isOk (s:_) | "OK" `isSuffixOf` tu s || "OK" `isPrefixOf` tu s = True
          isOk _ = False
          isError (s:_) | "ERROR" `isSuffixOf` tu s || "ERROR" `isPrefixOf` tu s = True
          isError ("Syntax Error, trying to recover and continue parse...":"ERROR":_) = True
          isError _ = False
          tu = map toUpper


--
-- * JVM back-end
--
objFileJVM f = dropExtension f 

runJVM ::  String -- libpath
         -> String -- ^ Java class file
	 -> FilePath -- ^ source file (for error reporting)
	 -> FilePath -- ^ known input file
	 -> FilePath -- ^ known output file
	 -> IO Bool

runJVM libPath classFile src inp outp = do
  let dir  = takeDirectory classFile
  d0  <- System.Directory.getCurrentDirectory
  setCurrentDirectory dir
  result <- runProg ("java -cp .:" ++ libPath ++ " " ++ takeBaseName classFile) src inp outp
  setCurrentDirectory d0
  return result

jvmBackend libpath = Backend { name = "JVM", objFile = objFileJVM, 
			 run = runJVM libpath }

--
-- * LLVM back-end
--
objFileLLVM f = dropExtension f <.> "bc"

runLLVM ::  String -- libpath
         -> String -- ^ LLVM bitcode file
	 -> FilePath -- ^ source file (for error reporting)
	 -> FilePath -- ^ known input file
	 -> FilePath -- ^ known output file
	 -> IO Bool

runLLVM libPath bcFile src inp outp = do
  let dir  = takeDirectory bcFile
  d0  <- System.Directory.getCurrentDirectory
  setCurrentDirectory dir
  system ("rm a.out")
  system ("llvm-ld " ++ bcFile ++ " " ++ libPath ++"/runtime.bc")
  result <- runProg "./a.out" src inp outp
  setCurrentDirectory d0
  return result

llvmBackend libpath = Backend { name = "LLVM", objFile = objFileLLVM, 
			 run = runLLVM libpath }

--
-- * x86 back-end
--

objFilex86 f = dropExtension f <.> "o"

runx86 libPath oFile src inp outp = do
  let dir  = takeDirectory oFile
  d0  <- System.Directory.getCurrentDirectory
  setCurrentDirectory dir
  system ("rm a.out")
  system ("gcc -m32 " ++ oFile ++ " " ++ libPath ++"/runtime.o")
  result <- runProg "./a.out" src inp outp
  setCurrentDirectory d0
  return result

x86Backend libpath = Backend { name = "x86", objFile = objFilex86,
                               run = runx86 libpath }

getTestFilesForPath :: String -> IO [String]
getTestFilesForPath f = do
  d <- doesDirectoryExist f
  if d then jlFiles f else do
    d' <- doesFileExist f
    if d' then return [f]
          else error $ "Not a file or directory: " ++ f
        
                                    

-- | Get all .jl files in the given directory.
jlFiles :: FilePath -> IO [FilePath]
jlFiles d = do fs <- getDirectoryContents d
               return $ map (d </>) $ sort $ filter ((".jl" ==) . takeExtension) fs


--
-- * Terminal output colors
--

type Color = Int

color :: Color -> String -> String
color c s = fgcol c ++ s ++ normal

highlight = "\ESC[7m"
bold      = "\ESC[1m"
underline = "\ESC[4m"
normal    = "\ESC[0m"
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"
bgcol col = "\ESC[0" ++ show (40+col) ++ "m"

red, green, blue :: Color
red = 1
green = 2
blue = 4
