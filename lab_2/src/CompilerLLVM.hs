{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CompilerLLVM where

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

-- "Abstract" LLVM Instructions
--   will be translated into strings later on
data LLVMInstruction = 
	Nop
	deriving (Show)
	
type MethodDefinition = (MethodLinkType, ([Type], Type))
data MethodLinkType = Internal MethodAttrib
                    | Private MethodAttrib
                    | External MethodAttrib
data MethodAttrib = ReadNone MethodCC
                  | ReadOnly MethodCC
                  | NoUnwind MethodCC
                  | NoAttrib MethodCC
data MethodCC = CCC
              | FastCC -- Calling convention

-- standard functions that are implemented in the Runtime class
-- all are marked "external" which when translated means
-- they are in the Runtime class
stdFuncs = [(Ident "printInt", (External (NoAttrib CCC), ([Int],Void)) ),
	    (Ident "readInt", (External (NoAttrib CCC), ([],Int)) ),
	    (Ident "printDouble", (External (NoAttrib CCC), ([Doub],Void)) ),
	    (Ident "readDouble", (External (NoAttrib CCC), ([],Doub)) )]

-- Compiler environment
data Env = Env {
		 classname :: String,
		 signatures :: Map Ident MethodDefinition,
		 variables :: [Map Ident (Integer, Type)],
		 nextVarIndex :: Integer,
		 nextLabelIndex :: Integer,
		 codeStack :: [LLVMInstruction],
		 programCode :: [[LLVMInstruction]],
		 compiledCode :: [String] }

-- Get next available label index
getLabel :: CP Integer
getLabel = do
	next_label <- gets nextLabelIndex
	modify (\e -> e { nextLabelIndex = next_label + 1} )
	return next_label

-- add variable to current context (i.e. give it a local var number)
addVar :: Type -> Ident -> CP ()
addVar t n = do
	v:vs <- gets variables
	next_index <- gets nextVarIndex
	case t of
		Doub -> modify (\e -> e { nextVarIndex = next_index + 2 })
		otherwise -> modify (\e -> e { nextVarIndex = next_index + 1 })
	when (Map.member n v) $ fail $ "adding a variable " ++ (show n) ++ " that is already in top context"
	let v' = Map.insert n (next_index, t) v
	modify (\e -> e { variables = v':vs } )

-- get local var number and type from variable name
getVar :: Ident -> CP (Integer, Type)
getVar n = do
	vars <- gets variables
	rtrn (catMaybes ((map (Map.lookup n) vars)))
  where
	-- if we cant find anything, make sure we fail
	  rtrn :: [(Integer, Type)] -> CP (Integer, Type)
	  rtrn [] = fail $ (show n) ++ " referenced, but not found!"
	  rtrn (x:xs) = return x

-- push a jasmine instruction on the code stack
putInstruction :: LLVMInstruction -> CP ()
putInstruction instr = undefined {-do
	code_stack <- gets codeStack
	modify (\e -> e { codeStack = code_stack ++ [instr] })
-}
-- clear context (i.e. enter a new method)
clearContexSpec :: CP ()
clearContexSpec = undefined {-do
	modify (\e -> e { variables = [Map.empty],
										nextVarIndex = 0,
										nextLabelIndex = 0,
	                  currentStackDepth = 0,
	                  maxStackDepth = 0,
	                  codeStack = []})
	-}

-- Create an empty environment
emptyEnv :: String -> Env
emptyEnv name = Env {
                 classname = name,
                 signatures = Map.empty, --Map.fromList stdFuncs,
                 variables = [Map.empty],
								 nextVarIndex = 0,
								 nextLabelIndex = 0,
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

-- main entry point for compiler
compile :: String -> Program -> Err [String]
compile n p = (evalStateT . unCPM) (compileTree p) $ emptyEnv n

-- compile expressions
compileExp :: Expr -> CP Type
compileExp expr = undefined {-do
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
			--mapM (\e -> decrStack) expList
			
			method_sig <- lookFun ident
			case method_sig of
				(Internal (args, ret)) -> do
					mapM argDecrStack args
					when (ret /= Void) incrStack -- only increase stack if we the function returns something
					clname <- gets classname
					putInstruction $ FunctionCall (clname ++ "/" ++ n) args ret
					return ret
					
				(External (args, ret)) -> do
					mapM argDecrStack args
					when (ret /= Void) incrStack -- only increase stack if we the function returns something
					putInstruction $ FunctionCallExternal n args ret
					return ret
				
				where
					argDecrStack :: Type -> CP ()
					argDecrStack t = case t of
						Doub -> do
							decrStack
							decrStack
						Int -> decrStack
						otherwise -> return ()
				
		EAppS (Ident "printString") str -> do
			incrStack
			putInstruction $ FunctionCallPrintString str
			return Void
			
		Neg expr		-> do
			t <- compileExp expr
			putInstruction $ Negation t
			return t
		Not expr		-> do
			t <- compileExp expr
			label_id_1 <- getLabel
			label_id_2 <- getLabel
			let label_yes = "lab" ++ (show label_id_1)
			let label_end = "lab" ++ (show label_id_2) 
			
			putInstruction $ IfEq label_yes
			putInstruction $ PushInt 0
			putInstruction $ Goto label_end
			putInstruction $ Label label_yes
			putInstruction $ PushInt 1
			putInstruction $ Label label_end			
			
			return t

		EMul e0 op e1		-> do
			t <- compileExp e0
			compileExp e1
			decrStack
			when (t == Doub) decrStack
			case op of 
				Times 		-> putInstruction $ Mul t
				Mod   		-> putInstruction $ Modu t
				otherwise 	-> putInstruction $ Divide t
			return t

		EAdd e0 op e1		-> do
			t <- compileExp e0
			compileExp e1
			decrStack
			when (t == Doub) decrStack
			case op of 
				Plus 		-> putInstruction $ Add t
				otherwise 	-> putInstruction $ Sub t
			return t
		ERel e0 op e1 		-> do
			t <- compileExp e0
			compileExp e1
			label_id_1 <- getLabel
			label_id_2 <- getLabel			

			let label_yes = "lab" ++ (show label_id_1)
			let label_end = "lab" ++ (show label_id_2)
			
			case t of
				Doub -> case op of
						EQU -> do
							putInstruction $ DoubCmpG
							decrStack
							decrStack
							decrStack
							putInstruction $ IfEq label_yes
						LE -> do
							putInstruction $ DoubCmpL
							decrStack
							decrStack
							putInstruction $ PushInt 0
							putInstruction $ IfCmp LE label_yes
						GE -> do
							putInstruction $ DoubCmpG
							decrStack
							decrStack
							putInstruction $ PushInt 0
							putInstruction $ IfCmp GE label_yes
						NE -> do
							putInstruction $ DoubCmpG
							decrStack
							decrStack
							decrStack
							putInstruction $ IfCmp NE label_yes
						GTH -> do
							putInstruction $ DoubCmpG
							decrStack
							decrStack
							putInstruction $ PushInt 1
							putInstruction $ IfCmp EQU label_yes
						LTH -> do
							putInstruction $ DoubCmpL
							decrStack
							decrStack
							putInstruction $ PushInt (-1)
							putInstruction $ IfCmp EQU label_yes
					
				otherwise -> do
					decrStack
					decrStack
					putInstruction $ IfCmp op label_yes
			putInstruction $ PushInt 0
			putInstruction $ Goto label_end
			putInstruction $ Label label_yes
			putInstruction $ PushInt 1
			putInstruction $ Label label_end 
			
			incrStack
			return t

		EAnd e0 e1		-> do
			label_id1 <- getLabel
			label_id2 <- getLabel
			let label_1 = "lab" ++ (show label_id1)
			let label_2 = "lab" ++ (show label_id2)
			
			t <- compileExp e0
			putInstruction $ IfEq label_1
			putInstruction $ PushInt 1
			compileExp e1
			putInstruction $ And
			putInstruction $ Goto label_2
			putInstruction $ Label label_1
			putInstruction $ PushInt 0
			putInstruction $ Label label_2
			
			decrStack
			return t
			
		EOr e0 e1		-> do
			label_id1 <- getLabel
			label_id2 <- getLabel
			let label_1 = "lab" ++ (show label_id1)
			let label_2 = "lab" ++ (show label_id2)
			
			t <- compileExp e0
			putInstruction $ IfNe label_1
			putInstruction $ PushInt 0
			compileExp e1
			putInstruction $ Or
			putInstruction $ Goto label_2
			putInstruction $ Label label_1
			putInstruction $ PushInt 1
			putInstruction $ Label label_2
			
			decrStack
			return t
-}

-- compile variable declarations
compileDecl :: Type -> Item -> CP ()
compileDecl t (NoInit ident) = undefined {- do
	addVar t ident
	(local,_) <- getVar ident
	case t of 
		Doub 	  -> do
			putInstruction $ PushDoub 0.0
			incrStack
			incrStack
			putInstruction $ (Store t local)
			decrStack
			decrStack
		otherwise -> do
			putInstruction $ PushInt 0
			incrStack
			putInstruction $ (Store t local)
			decrStack

-- variable declaration with initialization expression
compileDecl t (Init ident expr) = do
	compileExp expr
	addVar t ident
	(local,_) <- getVar ident
	putInstruction $ (Store t local)
-}


-- compile statements
compileStm :: Stmt -> CP ()
compileStm (SType typ stm) = undefined {- do
	case stm of
		Empty 			-> fail $ "Trying to compile empty statement."
		BStmt (Block stmts) 	-> do
			old_vars <- gets variables
			modify (\e -> e { variables = Map.empty : old_vars } )
			mapM_ compileStm stmts
			v:vs <- gets variables
			modify (\e -> e { variables = vs })
			
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
				putInstruction (Return Int)
			Doub -> do
				compileExp expr
				putInstruction (Return Doub)
			Bool -> do
				compileExp expr
				putInstruction (Return Int)
			otherwise -> undefined
		 
		VRet     		-> putInstruction (Return Void)
		   
		Cond expr stmt		-> do
			new_label_id <- getLabel
			let new_label = "lab" ++ (show new_label_id)
			
			-- compare expression
			compileExp expr
			
			case expr of
				ELitTrue -> compileStm stmt
				otherwise -> do
					putInstruction $ IfEq new_label
					compileStm stmt
					putInstruction $ Label new_label
			
		   
		CondElse  expr ifs els  -> do
			label_else_id <- getLabel
			label_end_id <- getLabel
			let label_else = "lab" ++ (show label_else_id)
			let label_end = "lab" ++ (show label_end_id)
			
			-- compile expression
			compileExp expr
			
			decrStack
			putInstruction $ IfEq label_else
			compileStm ifs
			code_stack <- gets codeStack
			let last_stm = last code_stack
			
			case last_stm of
				Return _ -> putInstruction Nop
				otherwise -> putInstruction $ Goto label_end
			putInstruction $ Label label_else
			compileStm els
			
			case last_stm of
				Return _ -> putInstruction Nop
				otherwise -> putInstruction $ Label label_end
		 
		While expr stmt		-> do
			label_id_1 <- getLabel
			label_id_2 <- getLabel
			let label_1 = "lab" ++ (show label_id_1)
			let label_2 = "lab" ++ (show label_id_2)

			putInstruction $ Goto label_2
			putInstruction $ Label label_1
			compileStm stmt
			putInstruction $ Label label_2
			compileExp expr	
			putInstruction $ IfNe label_1
  		 
		SExp exprs		-> do
			compileExp exprs
			return ()
-}

-- iterate all the statements in a function definition and compile them
compileDef :: TopDef -> CP ()
compileDef (FnDef retType (Ident name) args (Block stms)) = undefined {-do
	clearContexSpec
	
	putInstruction (Label "entry")
	
	-- add arguments as local vars
	mapM_ (addArgs) args
	
	-- compile each code statement
	mapM_ (compileStm) stms
	
	code_stack' <- gets codeStack
	when ((length code_stack') == 1) $ putInstruction $ Return Void
	
	code_stack <- gets codeStack
	prog_code <- gets programCode
	max_stack_depth <- gets maxStackDepth
	num_locals <- gets nextVarIndex
	
	let code_stack' = ((StartMethod name (map (\(Arg t (Ident _)) -> t) args) retType max_stack_depth num_locals) : code_stack) ++ [EndMethod]
	modify (\e -> e { programCode = prog_code ++ [code_stack'] })

	where
		addArgs (Arg t i) = addVar t i
-}

-- add a function definition
addDef :: TopDef -> CP ()
addDef (FnDef retType n as _) = do
	sigs <- gets signatures 
	let ts = map argToType as
	let sigs' = Map.insert n (External (NoAttrib CCC), (ts,retType)) sigs
	modify (\e -> e { signatures = sigs' } ) -- updates the state record signatures
	where 
		argToType :: Arg -> Type
		argToType (Arg t _) = t
	
-- Look for a function in the signatures
lookFun :: Ident -> CP MethodDefinition
lookFun fName = do
	mbtSig <- gets (Map.lookup fName. signatures)
	when (isNothing mbtSig) (fail $ "Unknown function name")
	return $ fromJust mbtSig


-- compile a program tree to LLVM asm code
compileTree :: Program -> CPM Err [String]
compileTree (Program defs) = do
	
	-- store defs
	--mapM addDef defs
	
	-- compile all function defines
	--mapM compileDef defs
	
	-- translate jasmine code to strings
	--pgm_code <- gets programCode
	--fail $ show pgm_code
	--mapM_ (transLLVMBlock) pgm_code
	
	-- compiledCode now has translated jasmine values
	compiled_code <- gets compiledCode
	return $ compiled_code

