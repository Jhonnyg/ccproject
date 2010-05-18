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
	| Return Type String              -- Return type register
	| ReturnLit Type String           -- Return type literal
	| ReturnVoid                      -- Return void
	| FunctionBegin String [Arg] Type -- FunctiosBegin name params returntype
	| FunctionEnd
	| Alloc Type String -- Alloc type register
	| StoreLit Type String String -- Store type literalvalue register
	| Store Type String String -- Store type fromreg toreg
        | Load Type String String -- Load type a b (%a = load type %b)
        | Add Type String String String -- Add type to_reg from_reg value
	deriving (Show)
--Add typ inc_reg tmp_reg "1"

{-data Value =
  Integer
  | Boolean
  | Double
  deriving (Show)-}
	
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
		 variables :: [Map Ident (String, Type)],
		 registers :: Map Ident Int,
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

-- new register
newRegister :: Ident -> CP String
newRegister ident@(Ident n) = do
  regs <- gets registers
  case Map.lookup ident regs of
    Just i -> do
      modify (\e -> e { registers = Map.adjust (+ 1) ident regs } )
      return $ n ++ (show (i + 1))
    Nothing -> do
      modify (\e -> e { registers = Map.insert ident 0 regs} )
      return $ n ++ "0"

-- add variable to current context (i.e. give it a local var number)
addVar :: Type -> Ident -> CP String
addVar t n = do
	v:vs <- gets variables
	when (Map.member n v) $ fail $ "adding a variable " ++ (show n) ++ " that is already in top context"
	new_reg_name <- newRegister n
	let v' = Map.insert n (new_reg_name, t) v
	modify (\e -> e { variables = v':vs } )
	return new_reg_name

-- get local var number and type from variable name
getVar :: Ident -> CP (String, Type) -- (regname, type)
getVar n = do
	vars <- gets variables
	rtrn (catMaybes ((map (Map.lookup n) vars)))
  where
	-- if we cant find anything, make sure we fail
	  rtrn :: [(String, Type)] -> CP (String, Type)
	  rtrn [] = fail $ (show n) ++ " referenced, but not found!"
	  rtrn (x:xs) = return x

-- push a jasmine instruction on the code stack
putInstruction :: LLVMInstruction -> CP ()
putInstruction instr = do
	code_stack <- gets codeStack
	modify (\e -> e { codeStack = code_stack ++ [instr] })
	
-- clear context (i.e. enter a new method)
clearContexSpec :: CP ()
clearContexSpec = do
	modify (\e -> e { variables = [Map.empty],
	                  codeStack = []})

-- Create an empty environment
emptyEnv :: String -> Env
emptyEnv name = Env {
                classname = name,
                signatures = Map.fromList stdFuncs,
                variables = [Map.empty],
                registers = Map.empty,
                nextVarIndex = 0,
                nextLabelIndex = 0,
                codeStack = [],
                programCode = [],
                compiledCode = [
                "declare void @printInt(i32 %x)",
                "declare void @printDouble(double %x)",
                "declare void @printString(i8* %x)",
                "declare i32 @readInt()",
                "declare double @readDouble()"
                ] }

-- main entry point for compiler
compile :: String -> Program -> Err [String]
compile n p = (evalStateT . unCPM) (compileTree p) $ emptyEnv n

-- compile expressions
compileExp :: Expr -> CP (Maybe String, Type)
compileExp expr = do
	case expr of
		EVar name -> do
			(reg, typ) <- getVar name
			return (Just reg, typ)
		otherwise -> fail $ "Trying to compile an unknown expression."
{-
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
compileDecl t (NoInit ident) = do -- variable declaration with NO initialization expression
	reg_name <- addVar t ident
	putInstruction $ Alloc t reg_name
	
compileDecl t (Init ident expr) = do -- variable declaration with initialization expression
  reg_name <- addVar t ident
  putInstruction $ Alloc t reg_name
  case expr of
    ELitInt i  -> putInstruction $ (StoreLit t (show i) reg_name)
    ELitDoub i -> putInstruction $ (StoreLit t (show i) reg_name)
    ELitTrue -> putInstruction $ (StoreLit t "1" reg_name)
    ELitFalse -> putInstruction $ (StoreLit t "0" reg_name)
    otherwise -> do
      (val, t') <- compileExp expr
      case val of
          Just reg_from -> putInstruction $ (Store t reg_from reg_name)
          Nothing -> fail $ "lol"

-- compile statements
compileStm :: Stmt -> CP ()
compileStm (SType typ stm) = do
    case stm of
      Empty 			     -> fail $ "Trying to compile empty statement."
      
      -- Block statement (add a new clean context)
      BStmt (Block stmts) 	-> do
        old_vars <- gets variables
        modify (\e -> e { variables = Map.empty : old_vars } )
        mapM_ compileStm stmts
        v:vs <- gets variables
        modify (\e -> e { variables = vs } )
  		
  		-- Return statements!	
      Ret (ELitInt i)   -> putInstruction $ (Return Int (show i))
      Ret (ELitTrue)    -> putInstruction $ (Return Bool "1")
      Ret (ELitFalse)   -> putInstruction $ (Return Bool "0")
      Ret (ELitDoub i)  -> putInstruction $ (Return Doub (show i))
      Ret expr          -> do
                    (val,typ) <- compileExp expr
                    case val of
                        Just reg_val -> do
                            ret_val <- newRegister (Ident "retval")
                            putInstruction $ Load typ ret_val reg_val
                            putInstruction $ Return typ ret_val --Return Type String
                            
                        Nothing -> fail $ "Return statement failed"

      VRet             -> putInstruction $ ReturnVoid
      
      Decl t itmList    -> mapM_ (compileDecl t) itmList
      Ass name expr     -> undefined
      Incr name         -> do
                    (reg,typ) <- getVar name
                    (tmp_reg) <- newRegister (Ident "tmp")
                    (inc_reg) <- newRegister (Ident "inc")
                    putInstruction $ Load typ tmp_reg reg
                    putInstruction $ Add typ inc_reg tmp_reg "1"
                    putInstruction $ Store typ inc_reg reg
                            
                            --Store Type String String
      --getVar :: Ident -> CP (String, Type)
      unknown -> fail $ "Trying to compile an unknown statement! " ++ (show stm)
        
--compileExp :: Expr -> CP (String, Type)

    {- do
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
compileDef (FnDef retType (Ident name) args (Block stms)) = do
  
  modify (\e -> e { registers = Map.empty }) -- Clear register context
  clearContexSpec -- Clear current "block" context
  
  putInstruction $ FunctionBegin name args retType
  mapM_ (compileStm) stms
  putInstruction $ FunctionEnd
  
  code_stack <- gets codeStack
  prog_code <- gets programCode
  modify (\e -> e { programCode = prog_code ++ [code_stack] })
{-do
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
	let sigs' = Map.insert n (Internal (NoAttrib CCC), (ts,retType)) sigs
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

typeToLLVMType :: Type -> String
typeToLLVMType Int = "i32"
typeToLLVMType Doub = "double"
typeToLLVMType Bool = "i2"
typeToLLVMType Void = "void"

transLLVMInstr :: LLVMInstruction -> String
transLLVMInstr instr = do
  case instr of
    FunctionBegin name p rettype -> "define " ++ typeToLLVMType(rettype) ++ " @" ++ name ++ "(" ++ transParlist(p) ++ ") {\nentry:" -- TODO: fix parameter list!
    FunctionEnd       -> "}\n"
    Return t reg      -> "  ret " ++ typeToLLVMType(t) ++ " %" ++ reg
    ReturnLit t lit   -> "  ret " ++ typeToLLVMType(t) ++ " " ++ lit
    ReturnVoid        -> "  ret void"
    Alloc t reg       -> "  %" ++ reg ++ " = alloca " ++ typeToLLVMType(t)
    StoreLit t v reg  -> "  store " ++ typeToLLVMType(t) ++ " " ++ v ++ ", " ++ typeToLLVMType(t) ++ "* %" ++ reg
    Store t reg1 reg2 -> "  store " ++ typeToLLVMType(t) ++ " %" ++ reg1 ++ ", " ++ typeToLLVMType(t) ++ "* %" ++ reg2
    Load t reg1 reg2 -> "  %" ++ reg1 ++ " = load " ++ typeToLLVMType(t) ++ "* %" ++ reg2   -- Load type a b (%a = load type %b)
    Add t reg1 reg2 val -> "  %" ++ reg1 ++ " = add " ++ typeToLLVMType(t) ++ " %" ++ reg2 ++ ", " ++ val
    --Add Type String String String -- Add type to_reg from_reg value
    otherwise -> fail $ "Trying to translate unknown instruction!"
  
  where
    -- translate a parameter list in a function
    transParlist :: [Arg] -> String
    transParlist []   = ""
    transParlist p@((Arg t (Ident n)):[]) = typeToLLVMType(t) ++ " %" ++ n
    transParlist p@((Arg t (Ident n)):ps) = typeToLLVMType(t) ++ " %" ++ n ++ ", " ++ transParlist(ps)


-- translate instructions into strings
transLLVMBlock :: [LLVMInstruction] -> CP ()
transLLVMBlock context = do
  let str_src = map transLLVMInstr context
  compiled_code <- gets compiledCode
  modify (\e -> e { compiledCode = compiled_code ++ str_src })

-- compile a program tree to LLVM asm code
compileTree :: Program -> CPM Err [String]
compileTree (Program defs) = do
	
	-- store defs
	--mapM addDef defs
	
	-- compile all function defines
	mapM compileDef defs
	
	-- translate jasmine code to strings
	pgm_code <- gets programCode
	--fail $ show pgm_code
	mapM_ (transLLVMBlock) pgm_code
	
	-- compiledCode now has translated jasmine values
	compiled_code <- gets compiledCode
	return $ compiled_code

