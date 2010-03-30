{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler where

import Absjavalette
import Printjavalette
import ErrM

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad (liftM2)

import Data.Map (Map)
import qualified Data.Map as Map

-- Encapsulate environment in a state monad as to enable error and state handling at the same time 
newtype CPM m a = CPM { unCPM :: StateT Env m a }
    deriving (Monad, MonadTrans, MonadState Env)

-- Type alias to increase readability
type CP a = CPM Err a

data JasminInstr = 
	VReturn
	| Return
	| StartMethod String [Type] Type Int Int
	| Goto String
	| Label Integer
	| EndMethod
	deriving (Show)


--data JasminProgram = [JasminInstr]
-- Replace [(from,to)]
data Env = Env { signatures :: Map Ident Type,
		 variables :: [Map Ident (Integer, Type)],
		 nextVarIndex :: Integer,
		 nextLabelIndex :: Integer,
		 currentStackDepth :: Integer,
		 maxStackDepth :: Integer,
		 codeStack :: [JasminInstr],
		 programCode :: [[JasminInstr]],
		 compiledCode :: [String] }

getLabel :: CP Integer
getLabel = do
	next_label <- gets nextLabelIndex
	modify (\e -> e { nextLabelIndex = next_label++}
	return next_label
	

putInstruction :: JasminInstr -> CP ()
putInstruction instr = do
	code_stack <- gets codeStack
	modify (\e -> e { codeStack = code_stack ++ [instr] })

clearContexSpec :: CP ()
clearContexSpec = do
	program_code <- gets programCode
	code_stack <- gets codeStack
	modify (\e -> e { variables = [Map.empty],
										nextVarIndex = 0,
	                  currentStackDepth = 0,
	                  maxStackDepth = 0,
	                  codeStack = [],
	                  programCode = code_stack : program_code})

{- -- all the type signatures from all the functions
                      signatures :: Map Ident ([Type], Type)
                    , contexts :: [Map Ident Type]
		    , returnType :: Type -}

stdFuncs = [(Ident "printInt", ([Int],Void)),
	    (Ident "readInt", ([],Int)),
	    (Ident "printDouble", ([Doub],Void)),
	    (Ident "readDouble", ([],Doub))]	

-- Create an empty environment
emptyEnv :: String -> Env
emptyEnv name = Env { signatures = Map.empty,--Map.fromList stdFuncs, -- add our standard functions here from start?
                 variables = [Map.empty],
								 nextVarIndex = 0,
								 nextLabelIndex = 0,
								 currentStackDepth = 0,
								 maxStackDepth = 0,
								 codeStack = [],
								 programCode = [],
								 compiledCode = [".source " ++ name ++ ".j",
								                ".class  public " ++ name,
								                ".super  java/lang/Object",
																"; standard initializer",
																".method public <init>()V",
																"   aload_0",
																"   invokenonvirtual java/lang/Object/<init>()V",
																"   return",
																".end method"] }

compile :: Program -> Err [String]
compile p = (evalStateT . unCPM) (compileTree p) $ emptyEnv "MyJavaletteClass"

-- place one empty context at the top of the stack
{-
pushContext :: TC ()
pushContext = modify (\e -> e { contexts = (Map.empty :  contexts e)})

-- remove topmost context from the stack
popContext :: TC ()
popContext = do
        ctxStack <- gets contexts
        when (null ctxStack) (error "popping from an empty context stack")
        modify (\e -> e { contexts = tail ctxStack })

-- add a variable to current context and fail if it already exists
addVar :: Ident -> Type -> TC ()
addVar n t = do
        env <- get
        let (c:cs) = contexts env
        when (Map.member n c) $ fail $ "adding a variable " ++ (show n) ++ " that is already in top context"
        let c' = Map.insert n t c
        let env' = env { contexts = (c' : cs) }
        put env'

-- add a function definition
addDef :: TopDef -> TC ()
addDef (FnDef retType n as _) = do
	sigs <- gets signatures 
	let ts = map argToType as
	let sigs' = Map.insert n (ts,retType) sigs
	modify (\e -> e { signatures = sigs' } ) -- updates the state record signatures
	where 
	argToType :: Arg -> Type
	argToType (Arg t _) = t

--addDef (FnDef Void printInt 
	
-- Look for a variable in allall the contextss
lookVar :: Ident -> TC Type
lookVar n = do
        ctxs <- gets contexts 
        rtrn (catMaybes ((map (Map.lookup n) ctxs)))
        where
	-- if we cant find anything, make sure we fail
        rtrn :: [Type] -> TC Type
        rtrn [] = fail $ "type of " ++ (show n) ++ " not found"
        rtrn (x:xs) = return x

-- Look for a function in the signatures
lookFun :: Ident -> TC ([Type], Type)
lookFun fName = do
	mbtSig <- gets (Map.lookup fName. signatures)
	when (isNothing mbtSig) (fail $ "Unknown function name")
	return $ fromJust mbtSig
	
-}
compileExp :: Expr -> CP ()
compileExp expr = do
	case expr of
		EVar name 		-> undefined
		ELitInt i 		-> undefined 
		ELitDoub d 		-> undefined
		ELitTrue		-> undefined
		ELitFalse		-> undefined
		EApp n expList 		-> undefined
		EAppS (Ident "printString") str -> undefined
		EAppS n str		-> undefined
		Neg expr		-> undefined
			-- exprVal <- compileExp expr
			-- push - exprVal to stack
		Not expr		-> undefined
		EMul e0 op e1		-> undefined
		EAdd e0 op e1		-> undefined
		ERel e0 (EQU) e1 	-> undefined
		ERel e0 op e1		-> undefined
		EAnd e0 e1		-> undefined
		EOr e0 e1		-> undefined
		
compileStm :: Stmt -> CP ()
compileStm stm = do
	--fail (show stm)
	case stm of
		SType t stmt 		-> compileStm stmt
		Empty 			-> undefined
		BStmt (Block stmts) 	-> undefined
			
		Decl  t itmList		-> undefined
		  			
		Ass name epxr		-> undefined
		     
		Incr name		-> undefined
		   
		Decr name		-> undefined
		   
		Ret  expr     		-> undefined
		 
		VRet     		-> putInstruction VReturn
		   
		Cond expr stmt		-> undefined
		   
		CondElse  expr ifs els  -> undefined
		 
		While expr stmt		-> undefined
  		 
		SExp exprs		-> undefined

-- add a variable to current context and fail if it already exists
addVar :: Type -> Ident -> CP ()
addVar t n = do
	env <- get
	let (v:vs) = variables env
	new_var_index <- gets nextVarIndex
	when (Map.member n v) $ fail $ "adding a variable " ++ (show n) ++ " that is already in top variable context"
	let v' = Map.insert n (new_var_index, t) v
	let env' = env { variables = (v' : vs), nextVarIndex = new_var_index + 1 }
	put env'


-- iterate all the statements in a function definition and compile them
compileDef :: TopDef -> CP ()
compileDef (FnDef retType (Ident name) args (Block stms)) = do
	clearContexSpec
	-- put method instructions!
	putInstruction (StartMethod name)
	
	mapM_ (addArgs) args
	mapM_ (compileStm) stms
	
	-- TODO: add stack information here!!!
	
	-- put method end instruction
	putInstruction (EndMethod)
	
	code_stack <- gets codeStack
	prog_code <- gets programCode
	modify (\e -> e { programCode = prog_code ++ [code_stack] })

	where
		addArgs (Arg t i) = addVar t i


-- translate a specific jasmine instruction to string
transJasmine :: JasminInstr -> String
transJasmine instr = do
	case instr of 
		VReturn -> "return"
		StartMethod name args rettype stack locals -> ".method public static " ++ name ++ "(" ++ "LOL" ++ ")V"
		EndMethod -> ".end method"
		otherwise -> "undefined"

-- translate a block of jasmine instructions and save result in state monad
transJasmineBlock :: [JasminInstr] -> CP ()
transJasmineBlock context = do
	let str_src = map transJasmine context
	compiled_code <- gets compiledCode
	modify (\e -> e { compiledCode = compiled_code ++ str_src })

compileTree (Program defs) = do
	
	-- compile all function defines
	mapM compileDef defs
	
	-- translate jasmine code to strings
	pgm_code <- gets programCode
	mapM_ (transJasmineBlock) pgm_code
	
	-- compiledCode now has translated jasmine values
	compiled_code <- gets compiledCode
	return $ compiled_code
