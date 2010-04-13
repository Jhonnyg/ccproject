{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler where

import Absjavalette
import Printjavalette
import ErrM

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad (liftM2)

import Data.Maybe (fromJust, catMaybes, isNothing)

import Data.Map (Map)
import qualified Data.Map as Map

import List (intersperse)

-- Encapsulate environment in a state monad as to enable error and state handling at the same time 
newtype CPM m a = CPM { unCPM :: StateT Env m a }
    deriving (Monad, MonadTrans, MonadState Env)

-- Type alias to increase readability
type CP a = CPM Err a

data JasminInstr = 
	VReturn
	| IReturn
	| DReturn
	| StartMethod String [Type] Type Integer Integer
	| Goto String
	| Label String
	| EndMethod
	| PushInt Integer
	| PushDoub Double
	| Store Type Integer
	| Load Type Integer
	| Increase Integer Integer
	| FunctionCall String [Type] Type
	| FunctionCallExternal String [Type] Type
	| Add Type
	| Sub Type
	deriving (Show)


data MethodSignature = Internal ([Type], Type) | External ([Type], Type)

--data JasminProgram = [JasminInstr]
-- Replace [(from,to)]
data Env = Env {
		 classname :: String,
		 signatures :: Map Ident MethodSignature,
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
	modify (\e -> e { nextLabelIndex = next_label + 1} )
	return next_label

addVar :: Type -> Ident -> CP ()
addVar t n = do
	v:vs <- gets variables
	next_index <- gets nextVarIndex
	when (Map.member n v) $ fail $ "adding a variable " ++ (show n) ++ " that is already in top context"
	let v' = Map.insert n (next_index, t) v
	modify (\e -> e { variables = v':vs, nextVarIndex = next_index + 1} )
	
getVar :: Ident -> CP (Integer, Type)
getVar n = do
	vars <- gets variables
	rtrn (catMaybes ((map (Map.lookup n) vars)))
  where
	-- if we cant find anything, make sure we fail
	  rtrn :: [(Integer, Type)] -> CP (Integer, Type)
	  rtrn [] = fail $ (show n) ++ " referenced, but not found!"
	  rtrn (x:xs) = return x

putInstruction :: JasminInstr -> CP ()
putInstruction instr = do
	code_stack <- gets codeStack
	modify (\e -> e { codeStack = code_stack ++ [instr] })

-- increase stack counter
incrStack :: CP ()
incrStack = do
	stack_depth <- gets currentStackDepth
	max_stack_depth <- gets maxStackDepth
	let stack_depth' = stack_depth + 1
	
	when (stack_depth' > max_stack_depth) $ modify (\e -> e { maxStackDepth = stack_depth' })
	modify (\e -> e { currentStackDepth = stack_depth'})
	
-- increase stack counter
decrStack :: CP ()
decrStack = do
	stack_depth <- gets currentStackDepth
	max_stack_depth <- gets maxStackDepth
	let stack_depth' = stack_depth - 1
	
	modify (\e -> e { currentStackDepth = stack_depth'})

clearContexSpec :: CP ()
clearContexSpec = do
--	program_code <- gets programCode
--	code_stack <- gets codeStack
	modify (\e -> e { variables = [Map.empty],
										nextVarIndex = 0,
	                  currentStackDepth = 0,
	                  maxStackDepth = 0,
	                  codeStack = []})
--	                  programCode = code_stack : program_code})

{- -- all the type signatures from all the functions
                      signatures :: Map Ident ([Type], Type)
                    , contexts :: [Map Ident Type]
		    , returnType :: Type -}

stdFuncs = [(Ident "printInt", External ([Int],Void)),
	    (Ident "readInt", External ([],Int)),
	    (Ident "printDouble", External ([Doub],Void)),
	    (Ident "readDouble", External ([],Doub))]	

-- Create an empty environment
emptyEnv :: String -> Env
emptyEnv name = Env {
                 classname = name,
                 signatures = Map.fromList stdFuncs, -- add our standard functions here from start?
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
																".end method",
																".method public static main([Ljava/lang/String;)V",
																"   .limit locals 1",
																"   invokestatic " ++ name ++ "/main()I",
																"   pop",
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

-- compile expressions
compileExp :: Expr -> CP Type
compileExp expr = do
	case expr of
		EVar name 		-> do
			incrStack
			(local, typ) <- getVar name
			when (typ == Doub) incrStack
			putInstruction (Load typ local)
			return typ
		ELitInt i 		-> do
			incrStack
			putInstruction (PushInt i)
			return Int
			
		ELitDoub d 		-> 	do
			incrStack
			incrStack
			putInstruction (PushDoub d)
			return Doub
			
		ELitTrue		-> do
			incrStack
			putInstruction (PushInt 1)
			return Int
			
		ELitFalse		->	 do
			incrStack
			putInstruction (PushInt 0)
			return Int
			
		EApp ident@(Ident n) expList 		-> do
			mapM compileExp expList
			mapM (\e -> decrStack) expList
			
			method_sig <- lookFun ident
			case method_sig of
				(Internal (args, ret)) -> do
					when (ret /= Void) incrStack -- only increase stack if we the function returns something
					clname <- gets classname
					putInstruction $ FunctionCall (clname ++ "/" ++ n) args ret
					return ret
					
				(External (args, ret)) -> do
					when (ret /= Void) incrStack -- only increase stack if we the function returns something
					putInstruction $ FunctionCallExternal n args ret
					return ret
				
		EAppS (Ident "printString") str -> undefined
		Neg expr		-> undefined
			-- exprVal <- compileExp expr
			-- push - exprVal to stack
		Not expr		-> undefined
		EMul e0 op e1		-> undefined
		EAdd e0 op e1		-> do
			t <- compileExp e1
			compileExp e0
			decrStack
			when (t == Doub) decrStack
			case op of 
				Plus -> do
								putInstruction $ Add t
				otherwise -> do
								putInstruction $ Sub t
			return t
		ERel e0 (EQU) e1 	-> undefined
		ERel e0 op e1		-> undefined
		EAnd e0 e1		-> undefined
		EOr e0 e1		-> undefined

-- compile variable declarations
compileDecl :: Type -> Item -> CP ()
compileDecl t (NoInit ident) = addVar t ident
compileDecl t (Init ident expr) = do
	addVar t ident
	(local,_) <- getVar ident
	compileExp expr
	putInstruction $ (Store t local)


-- compile statements
compileStm :: Stmt -> CP ()
compileStm (SType typ stm) = do
	case stm of
		Empty 			-> fail $ "Trying to compile empty statement."--undefined
		BStmt (Block stmts) 	-> mapM_ compileStm stmts
			
		Decl  t itmList		-> mapM_ (compileDecl t) itmList
		  			
		Ass name expr		-> do
			compileExp expr
			(local, typ) <- getVar name
			putInstruction $ Store typ local
		     
		Incr name		-> do
			(local, _) <- getVar name
			putInstruction $ Increase local 1
		   
		Decr name		-> 	do
				(local, _) <- getVar name
				putInstruction $ Increase local (-1)
		   
		Ret  expr     		-> case typ of
			Int -> do
				compileExp expr
				putInstruction IReturn
			Doub -> do
				compileExp expr
				putInstruction DReturn
			Bool -> do
				compileExp expr
				putInstruction IReturn
			otherwise -> undefined
		 
		VRet     		-> putInstruction VReturn
		   
		Cond expr stmt		-> undefined
		   
		CondElse  expr ifs els  -> undefined
		 
		While expr stmt		-> undefined
  		 
		SExp exprs		-> do
			compileExp exprs
			return ()


-- iterate all the statements in a function definition and compile them
compileDef :: TopDef -> CP ()
compileDef (FnDef retType (Ident name) args (Block stms)) = do
	clearContexSpec
	
	putInstruction (Label "entry")
	
	mapM_ (addArgs) args
	mapM_ (compileStm) stms
	
	-- TODO: add stack information here!!!
	
	-- put method instructions!
	--putInstruction (StartMethod name (map (\(Arg t (Ident _)) -> t) args) retType 0 0) -- StartMethod name args rettype stack locals
	-- put method end instruction
	--putInstruction (EndMethod)
	code_stack <- gets codeStack
	prog_code <- gets programCode
	max_stack_depth <- gets maxStackDepth
	num_locals <- gets nextVarIndex
	let code_stack' = ((StartMethod name (map (\(Arg t (Ident _)) -> t) args) retType max_stack_depth num_locals) : code_stack) ++ [EndMethod]
	modify (\e -> e { programCode = prog_code ++ [code_stack'] })

	where
		addArgs (Arg t i) = addVar t i

-- add a function definition
addDef :: TopDef -> CP ()
addDef (FnDef retType n as _) = do
	sigs <- gets signatures 
	let ts = map argToType as
	let sigs' = Map.insert n (Internal (ts,retType)) sigs
	modify (\e -> e { signatures = sigs' } ) -- updates the state record signatures
	where 
		argToType :: Arg -> Type
		argToType (Arg t _) = t
	
-- Look for a function in the signatures
lookFun :: Ident -> CP MethodSignature--([Type], Type)
lookFun fName = do
	mbtSig <- gets (Map.lookup fName. signatures)
	when (isNothing mbtSig) (fail $ "Unknown function name")
	return $ fromJust mbtSig

-- translate types to jasmine type identifiers
transJasmineType :: Type -> String
transJasmineType Int = "I"
transJasmineType Doub = "D"
transJasmineType Bool = "I"
transJasmineType Void = "V"

-- translate a specific jasmine instruction to string
transJasmine :: JasminInstr -> String
transJasmine instr = do
	case instr of 
		IReturn -> "  ireturn"
		DReturn -> "  dreturn"
		VReturn -> "  return"
		StartMethod name args rettype stack locals -> "\n.method public static " ++ name ++ "(" ++ (intersperse ',' (concat (map transJasmineType args)) ) ++ ")" ++ (transJasmineType rettype) ++ 
																									"\n  .limit locals " ++ (show locals) ++
																									"\n  .limit stack " ++ (show stack)
		EndMethod -> ".end method"
		PushInt i -> "  ldc " ++ (show i)
		PushDoub d -> "  ldc2_w " ++ (show d)
		FunctionCall n args ret -> "  invokestatic " ++ (map (\e -> if (e == '.') then '/' else e) n) ++ "(" ++ (intersperse ',' (concat (map transJasmineType args)) ) ++ ")" ++ (transJasmineType ret)
		FunctionCallExternal n args ret -> "  invokestatic Runtime/" ++ n ++ "(" ++ (intersperse ',' (concat (map transJasmineType args)) ) ++ ")" ++ (transJasmineType ret)
		Label lbl -> " " ++ lbl ++ ":"
		Store typ i -> case typ of
			Int -> "  istore " ++ (show i)
			Doub -> "  dstore " ++ (show i)
			Bool -> "  istore " ++ (show i)
			otherwise -> fail $ "Store instruction failed, unsupported type!"
		Load typ i -> case typ of
			Int -> "  iload " ++ (show i)
			Doub -> "  dload " ++ (show i)
			Bool -> "  iload " ++ (show i)
		Increase local i -> "  iinc " ++ (show local) ++ " " ++ (show i)
		Add typ -> case typ of 
			Int -> "  iadd"
			Doub -> "  dadd"
			otherwise -> fail $ "No add operator for " ++ (show typ)
		Sub typ -> case typ of 
			Int -> "  isub"
			Doub -> "  dsub"
			otherwise -> fail $ "No subtract operator for " ++ (show typ)
		otherwise -> "undefined"

-- translate a block of jasmine instructions and save result in state monad
transJasmineBlock :: [JasminInstr] -> CP ()
transJasmineBlock context = do
	let str_src = map transJasmine context
	compiled_code <- gets compiledCode
	modify (\e -> e { compiledCode = compiled_code ++ str_src })

compileTree :: Program -> CPM Err [String]
compileTree (Program defs) = do
	
	-- store defs
	mapM addDef defs
	
	-- compile all function defines
	mapM compileDef defs
	
	-- translate jasmine code to strings
	pgm_code <- gets programCode
	--fail $ show pgm_code
	mapM_ (transJasmineBlock) pgm_code
	
	-- compiledCode now has translated jasmine values
	compiled_code <- gets compiledCode
	return $ compiled_code
