module Grade where

import RunCommand
import KompTest

import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.FilePath
import System.Posix.Files
import Control.Exception
import Control.Monad
import System.Console.GetOpt

cmd c = do
  putStrLn c
  (out,err,code) <- runCommandStrWait c ""
  putStrLn out
  putStrLn err


makeAbsolute p = do
  c <- getCurrentDirectory
  return (c </> p)

maybeBuild _ [] = return ()
maybeBuild groupPath0 ((tarOpt,subm) : _) = do
   cmd $ "tar -C "++groupPath0 ++" -"++ tarOpt++"xvf "++ show (groupPath0 </> subm)
   cmd $ "make -C " ++ show (groupPath0 </> "src")
   
testAll compiler bs exts [testSuitePath00, groupPath0] = do
  allFiles <- getDirectoryContents groupPath0
  let submissions = [(opts, s) | 
                    (opts, suff) <- [("z", ".tar.gz"), ("j", ".tar.bz2"), ("j", ".tar.bzip2"), ("", ".tar")],
                    s <- filter (suff `isSuffixOf`) allFiles]
  maybeBuild groupPath0 submissions
  let testSuitePath0 = groupPath0 </> "graderTestSuite"
  cmd $ "rm -r " ++ testSuitePath0
  cmd $ "cp -R " ++ testSuitePath00 </> "examples" ++ " " ++ testSuitePath0
  let exePath0 = groupPath0 </> compiler

  exePath <- makeAbsolute exePath0
  groupPath <- makeAbsolute groupPath0
 
  testSuitePath <- makeAbsolute testSuitePath0
  curDir <- getCurrentDirectory
  let exeDir = takeDirectory exePath
  putStrLn $ "Running tests for " ++ exePath

  let libpath = groupPath </> "lib"
  let testProg = if null bs then Nothing
                 else case bs of
                   "JVM"  : _ -> Just testJVM
                   "LLVM" : _ -> Just testLLVM
                   "x86" : _ -> Just testx86
                   b : _ -> error ("Unknown backend: " ++ b) 
  setCurrentDirectory exeDir
  summary <- forM (testSpecs testProg exts libpath) $ \(points, name, tests) -> do
    putStrLn $ name ++ "..."
    results <- forM tests $ \(good, p, testFunction) -> do
        testFiles <- getTestFilesForPath (testSuitePath </> p)
        putStrLn $ p ++ "..."
        rs <- testFunction exePath good testFiles
        report p rs 
        return (p, rs)
    putStrLn $ "Passed suites: " ++ (concat $ intersperse ", " $ [p | (p,rs) <- results, and rs])
    let tally = concat (map snd results)
    return (name, if and tally then points else (0 :: Int), tally)

  setCurrentDirectory curDir

  putStrLn $ "Summary:\n" ++ unlines (map summaryLine summary)
  putStrLn $ "Credits total: " ++ show (sum [x | (_,x,_) <- summary])

padl n s = replicate (n - length s) ' ' ++ s


summaryLine (name, points, tests) = 
  padl 2 (show points) ++ " " ++ name ++ " " ++ "(" ++ show (length (filter id tests)) ++ "/" ++ show (length tests) ++ ")"

testSpecs testProg exts libpath = 
  (0, "Compiling core programs",     [(True, "good",testCompilation), (False, "bad", testCompilation)]) : 
  map (\x -> (0,"Compiling extension " ++ x,[(True,"extensions/"++x,testCompilation)])) exts ++ 
  case testProg of   
     Nothing -> []
     Just backEnd ->
      (0, "Running core programs",        [(True, "good",backEnd libpath)]) : 
      map (\x -> (6,"Running extension " ++ x,[(True,"extensions/"++x,backEnd libpath)])) exts 



testBack back cmd good fs = if good then test cmd fs back else return []

testJVM classpath = testBack (jvmBackend classpath)
testLLVM libpath  = testBack (llvmBackend libpath)
testx86 libpath   = testBack (x86Backend libpath)

data Flag = SearchScript String
          | Extension String
          | Back String
   deriving (Eq,Ord)

flags =
    [Option "s" ["search-compiler"] (ReqArg SearchScript "<compiler>") "search for the specified compiler",
     Option "x" ["extension"] (ReqArg Extension "<extension>") "specify extensions to test",
     Option "b" ["backend"] (ReqArg Back "<backend>") "specify backend"]

main = do
  argv <- getArgs
  case getOpt Permute flags argv of
    (opts,args,[]) -> do
         let searchList0 = [s | SearchScript s <- opts]
             compiler = if null searchList0 then "jlc" else head searchList0
             exts = [e | Extension e <- opts]
             bs = [b | Back b <- opts]
         testAll compiler bs exts args
    (_,_,errs) -> do
            hPutStrLn stderr (concat errs ++ usageInfo "" flags)
            exitWith (ExitFailure 1)

 where defaultSearchList = [] 
