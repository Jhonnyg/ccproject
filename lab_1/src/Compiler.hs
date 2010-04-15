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
	Return Type
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
	| FunctionCallPrintString String
	| Add Type
	| Sub Type
	| Mul Type
	| Modu Type
	| Divide Type
	| And
	| Or
	| IfEq String
	| IfCmp RelOp String
	| IfNe String
	| DoubCmpG
	| DoubCmpL
	| Negation Type
	| Pop
	| Nop
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

-- clear context (i.e. enter a new method)
clearContexSpec :: CP ()
clearContexSpec = do
	modify (\e -> e { variables = [Map.empty],
										nextVarIndex = 0,
										nextLabelIndex = 0,
	                  currentStackDepth = 0,
	                  maxStackDepth = 0,
	                  codeStack = []})

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

-- main entry point for compiler
compile :: String -> Program -> Err [String]
compile n p = (evalStateT . unCPM) (compileTree p) $ emptyEnv n

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

-- compile variable declarations
compileDecl :: Type -> Item -> CP ()
compileDecl t (NoInit ident) = do
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


-- iterate all the statements in a function definition and compile them
compileDef :: TopDef -> CP ()
compileDef (FnDef retType (Ident name) args (Block stms)) = do
	clearContexSpec
	
	putInstruction (Label "entry")
	
	-- add arguments as local vars
	mapM_ (addArgs) args
	
	-- compile each code statement
	mapM_ (compileStm) stms
	
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
		Return Int -> "  ireturn"
		Return Doub -> "  dreturn"
		Return Void -> "  return"
		StartMethod name args rettype stack locals -> "\n.method public static " ++ name ++ "(" ++ ( (concat (map transJasmineType args)) ) ++ ")" ++ (transJasmineType rettype) ++ 
																									"\n  .limit locals " ++ (show locals) ++
																									"\n  .limit stack " ++ (show stack)
		EndMethod -> ".end method"
		PushInt i -> "  ldc " ++ (show i)
		PushDoub d -> "  ldc2_w " ++ (show d)
		FunctionCall n args ret -> "  invokestatic " ++ (map (\e -> if (e == '.') then '/' else e) n) ++ "(" ++ ( (concat (map transJasmineType args)) ) ++ ")" ++ (transJasmineType ret)
		FunctionCallExternal n args ret -> "  invokestatic Runtime/" ++ n ++ "(" ++ (intersperse ',' (concat (map transJasmineType args)) ) ++ ")" ++ (transJasmineType ret)
		FunctionCallPrintString str -> "  ldc " ++ (show str) ++
																	"\n  invokestatic Runtime/printString(Ljava/lang/String;)V"
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
		Mul typ -> case typ of
			Int -> "  imul"
			Doub -> "  dmul"
			otherwise -> fail $ "No multiplication operator for " ++ (show typ)
		Modu typ -> case typ of 
			Int -> "  irem"
			Doub -> "  drem"
		Divide typ -> case typ of
			Doub -> "  ddiv"
			otherwise -> "  idiv"
		And -> "  iand"
		Or  -> "  ior"
		Goto lbl -> "  goto " ++ lbl
		IfEq lbl -> "  ifeq " ++ lbl
		IfNe lbl -> "  ifne " ++ lbl
		IfCmp op lbl -> case op of 
			EQU -> "  if_icmpeq " ++ lbl
			GTH -> "  if_icmpgt " ++ lbl
			LTH -> "  if_icmplt " ++ lbl
			NE -> "  if_icmpne " ++ lbl
			GE -> "  if_icmpge " ++ lbl
			LE -> "  if_icmple " ++ lbl
		DoubCmpG -> "  dcmpg"
		DoubCmpL -> "  dcmpl"
		Negation typ -> case typ of
			Int -> "  ineg"
			Doub -> "  dneg"
			otherwise -> fail $ "Unable to negate type " ++ (show typ)
		Pop -> "  pop"
		Nop -> ""

-- translate a block of jasmine instructions and save result in state monad
transJasmineBlock :: [JasminInstr] -> CP ()
transJasmineBlock context = do
	let str_src = map transJasmine context
	compiled_code <- gets compiledCode
	modify (\e -> e { compiledCode = compiled_code ++ str_src })

-- compile a program tree to jasmin code
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
