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

type Register = (String, Bool)

-- "Abstract" LLVM Instructions
--   will be translated into strings later on
data LLVMInstruction =
	Nop
	| Return Type Register              -- Return type register
	| ReturnLit Type String           -- Return type literal
	| ReturnVoid                      -- Return void
	| FunctionBegin String [(Register, Type)] Type -- FunctiosBegin name params returntype
	| FunctionEnd
	| Alloc Type Register -- Alloc type register
	| StoreLit Type String Register -- Store type literalvalue register
	| Store Type Register Register -- Store type fromreg toreg
	| Load Type Register Register -- Load type a b (%a = load type %b)
	| AddLit AddOp Type Register String String -- Add type to_reg value2 value1
	| Add AddOp Type Register Register String -- Add type to_reg reg1 value
	| AddRegs AddOp Type Register Register Register -- Add type to_reg reg1 reg2
	| Label String LLVMInstruction
	| IfCmp RelOp Type Register Register Register
	| BrCond Register String String
	| BrUnCond String
    | Negation Type Register Register -- Negation trgt_reg src_reg
	| FuncCall Ident Type [(Register, Type)] Register -- reg = call rettype @name(i32 %t0)
    | Mul Type Register Register Register
    | Modulus Type Register Register Register
    | Division Type Register Register Register
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
		 variables :: [Map Ident (Register, Type)],
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
newRegister :: Ident -> Bool -> CP Register -- register "ident" -> pointer -> (register name, pointer)
newRegister ident@(Ident n) pointer = do
  regs <- gets registers
  case Map.lookup ident regs of
    Just i -> do
      modify (\e -> e { registers = Map.adjust (+ 1) ident regs } )
      let reg_name = "%" ++ n ++ (show (i + 1))
      return (reg_name, pointer)
    Nothing -> do
      modify (\e -> e { registers = Map.insert ident 0 regs} )
      let reg_name = "%" ++ n ++ "0"
      return (reg_name, pointer)

-- add variable to current context (i.e. give it a local var number)

addVar :: Type -> Ident -> CP Register
addVar t n = do
	v:vs <- gets variables
	when (Map.member n v) $ fail $ "adding a variable " ++ (show n) ++ " that is already in top context"
	new_reg@(new_reg_name, _) <- newRegister n True
	let v' = Map.insert n (new_reg, t) v
	modify (\e -> e { variables = v':vs } )
	return new_reg

addVarNonPointer :: Type -> Ident -> CP Register
addVarNonPointer t n = do
	v:vs <- gets variables
	when (Map.member n v) $ fail $ "adding a variable " ++ (show n) ++ " that is already in top context"
	new_reg@(new_reg_name, _) <- newRegister n False
	let v' = Map.insert n (new_reg, t) v
	modify (\e -> e { variables = v':vs } )
	return new_reg

-- get local var number and type from variable name
getVar :: Ident -> CP (Register, Type) -- (regname, type)
getVar n = do
	vars <- gets variables
	rtrn (catMaybes ((map (Map.lookup n) vars)))
  where
	-- if we cant find anything, make sure we fail
	  rtrn :: [(Register, Type)] -> CP (Register, Type)
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
compileExp :: Expr -> CP (Maybe Register, Type)
compileExp expr = do
    case expr of
        EVar name -> do
            (reg, typ) <- getVar name
            case reg of
                (r, True) -> do
                    t_reg <- newRegister (Ident "tmp") False
                    putInstruction $ Load typ t_reg reg
                    return (Just t_reg, typ)
                (r, False) -> return (Just reg, typ)
        
        ELitInt i -> do
            t_reg <- newRegister (Ident "tmp") False
            putInstruction $ AddLit Plus Int t_reg "0" (show i)
            return (Just t_reg, Int)
        ELitDoub d -> do
            t_reg <- newRegister (Ident "tmp") False
            putInstruction $ AddLit Plus Doub t_reg "0.0" (show d)
            return (Just t_reg, Doub)
        ELitTrue -> do
            t_reg <- newRegister (Ident "tmp") False
            putInstruction $ AddLit Plus Bool t_reg "0" "1"
            return (Just t_reg, Bool)
        ELitFalse -> do
            t_reg <- newRegister (Ident "tmp") False
            putInstruction $ AddLit Plus Bool t_reg "0" "0"
            return (Just t_reg, Bool)
        EAdd e0 op e1 -> do
            (Just reg0, t) <- compileExp e0
            (Just reg1, _) <- compileExp e1
            t_reg <- newRegister (Ident "tmp") False
            putInstruction $ AddRegs op t t_reg reg0 reg1
            return (Just t_reg, t)
            
        EApp ident@(Ident n) expList -> do
            regs <- mapM compileExp expList
            let regs' = tidyRegs regs
            method_sig@(mlinktyp, (_, ret_t)) <- lookFun ident
            t_reg <- newRegister (Ident "tmp") False
            putInstruction $ FuncCall ident ret_t regs' t_reg
            return (Just t_reg, ret_t)
            {-case mlinktyp of
                Internal _ -> do
                    t_reg <- newRegister (Ident "tmp") False
                    putInstruction $ FuncCall ident ret_t regs' t_reg
                    return (Just t_reg, ret_t)
                otherwise  -> fail $ "Unknown function call!"
                -}
        ERel e0 op e1 -> do
            (Just reg0, t) <- compileExp e0
            (Just reg1, _) <- compileExp e1
            t_reg <- newRegister (Ident "tmp") False
            
            putInstruction $ IfCmp op t t_reg reg0 reg1
            
            return (Just t_reg, Bool)
        Not expr		-> do
            (Just reg, t) <- compileExp expr
        
            t_reg <- newRegister (Ident "tmp") False
            putInstruction $ Negation t t_reg reg
            return (Just t_reg,t)
        Neg expr		-> do
            (Just reg,t) <- compileExp expr
            t_reg <- newRegister (Ident "tmp") False
            lit_reg <- newRegister (Ident "tmp") False
            putInstruction $ AddLit Plus Int lit_reg "0" "-1"
            putInstruction $ Mul t t_reg reg lit_reg  
            
            return (Just t_reg,t)
        EMul e0 op e1		-> do
            (Just reg0,t) <- compileExp e0
            (Just reg1,_) <- compileExp e1
            
            t_reg <- newRegister (Ident "tmp") False
            case op of
                Times -> putInstruction $ Mul t t_reg reg0 reg1
                Div -> putInstruction $ Division t t_reg reg0 reg1
                Mod -> putInstruction $ Modulus t t_reg reg0 reg1
            return (Just t_reg,t)
        EAnd e0 e1		-> do
            end_label_id <- getLabel
            cond_label_id <- getLabel
            true_label_id <- getLabel
            false_label_id <- getLabel
            let cond_label = "lab" ++ (show cond_label_id)
            let end_label = "lab" ++ (show end_label_id)
            let true_label = "lab" ++ (show true_label_id)
            let false_label = "lab" ++ (show false_label_id)

            tobool_reg1 <- newRegister (Ident "tobool") False
            tobool_reg2 <- newRegister (Ident "tobool") False
            tmp_val_reg <- newRegister (Ident "tmp_val") False
            t_reg <- newRegister (Ident "t_reg") False
            true_reg <- newRegister (Ident "true_reg") False
            false_reg <- newRegister (Ident "false_reg") False
            return_reg <- newRegister (Ident "return_reg") True

            
            (Just reg0,t) <- compileExp e0
            (Just reg1,_) <- compileExp e1
            
            putInstruction $ Alloc t return_reg
            putInstruction $ AddLit Plus Bool tmp_val_reg "0" "0"
            putInstruction $ IfCmp NE t tobool_reg1 reg0 tmp_val_reg -- save result to tobool_reg
            putInstruction $ BrCond tobool_reg1 cond_label false_label
            
            putInstruction $ Label cond_label Nop--(IfCmp NE t tobool_reg2 reg1 tmp_val_reg)
            putInstruction $ IfCmp NE t tobool_reg2 reg1 tmp_val_reg
            putInstruction $ BrCond tobool_reg2 true_label false_label
            -- TEST exp2
            
            putInstruction $ Label true_label (AddLit Plus Bool true_reg "0" "1")
            putInstruction $ Store t true_reg return_reg 
            putInstruction $ BrUnCond end_label
            
            putInstruction $ Label false_label (AddLit Plus Bool false_reg "0" "0")
            putInstruction $ Store t false_reg return_reg 
            putInstruction $ BrUnCond end_label

            putInstruction $ Label end_label Nop            
            
            return (Just return_reg,Bool)
            
        otherwise -> do
            fail $ show expr
            
	where
		tidyRegs :: [(Maybe Register, Type)] -> [(Register, Type)]
		tidyRegs [] = []
		tidyRegs ((Just r, t):rs) = (r, t):tidyRegs(rs)
{-
--		otherwise -> fail $ "Trying to compile an unknown expression."
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
    ELitTrue   -> putInstruction $ (StoreLit t "1" reg_name)
    ELitFalse  -> putInstruction $ (StoreLit t "0" reg_name)
    otherwise  -> do
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
        Ret (ELitInt i)   -> putInstruction $ (ReturnLit Int (show i))
        Ret (ELitTrue)    -> putInstruction $ (ReturnLit Bool "1")
        Ret (ELitFalse)   -> putInstruction $ (ReturnLit Bool "0")
        Ret (ELitDoub i)  -> putInstruction $ (ReturnLit Doub (show i))
        Ret expr          -> do
            (val,typ) <- compileExp expr
            case val of
                Just reg_val -> do
                    --ret_val <- newRegister (Ident "retval") False
                    --putInstruction $ Load typ ret_val reg_val
                    putInstruction $ Return typ reg_val --Return Type String
                Nothing -> fail $ "Return statement failed"
        
        VRet             -> putInstruction $ ReturnVoid
        
        Decl t itmList    -> mapM_ (compileDecl t) itmList
        Decr name         -> do
            (reg,typ) <- getVar name
            (tmp_reg) <- newRegister (Ident "tmp") False
            (inc_reg) <- newRegister (Ident "inc") False
            putInstruction $ Load typ tmp_reg reg
            putInstruction $ Add Plus typ inc_reg tmp_reg "-1"
            putInstruction $ Store typ inc_reg reg
                      
        Incr name         -> do
            (reg,typ) <- getVar name
            (tmp_reg) <- newRegister (Ident "tmp") False
            (inc_reg) <- newRegister (Ident "inc") False
            putInstruction $ Load typ tmp_reg reg
            putInstruction $ Add Plus typ inc_reg tmp_reg "1"
            putInstruction $ Store typ inc_reg reg
                              
        Ass name expr     -> do
            (val,exp_typ) <- compileExp expr
            (reg,var_typ) <- getVar name
            
            case val of
                Just reg_val -> putInstruction $ Store var_typ reg_val reg    -- store tmp val to reg
                Nothing      -> fail "yep!"
        Cond expr stmt		-> do
            end_label_id <- getLabel
            then_label_id <- getLabel
            let end_label = "lab" ++ (show end_label_id)
            let then_label = "lab" ++ (show then_label_id)
            (val,typ) <- compileExp expr
            case val of
                Just reg@(_,ptr)    -> do
                    tmp_val_reg <- newRegister (Ident "tmp") False
                    tobool_reg <- newRegister (Ident "tobool") False
                    putInstruction $ AddLit Plus Bool tmp_val_reg "0" "0"
                    putInstruction $ IfCmp NE typ tobool_reg reg tmp_val_reg -- save result to tobool_reg
                    putInstruction $ BrCond tobool_reg then_label end_label
                    putInstruction $ Label then_label Nop
                    compileStm stmt                    
                    putInstruction $ BrUnCond end_label
                    putInstruction $ Label end_label Nop
                Nothing     -> fail $ "if fail" 
        CondElse  expr ifs els  -> do
            end_label_id <- getLabel
            then_label_id <- getLabel
            else_label_id <- getLabel
            let end_label = "lab" ++ (show end_label_id)
            let then_label = "lab" ++ (show then_label_id)
            let else_label = "lab" ++ (show else_label_id)
            (val,typ) <- compileExp expr
            case val of
                Just reg@(_,ptr)    -> do
                    tmp_val_reg <- newRegister (Ident "tmp") False
                    tobool_reg <- newRegister (Ident "tobool") False
                    putInstruction $ AddLit Plus Bool tmp_val_reg "0" "0"
                    putInstruction $ IfCmp NE typ tobool_reg reg tmp_val_reg -- save result to tobool_reg
                    putInstruction $ BrCond tobool_reg then_label else_label
                    putInstruction $ Label then_label Nop
                    compileStm ifs
                    putInstruction $ BrUnCond end_label
                    
                    putInstruction $ Label else_label Nop
                    compileStm els
                    
                    putInstruction $ BrUnCond end_label
                    putInstruction $ Label end_label Nop
                Nothing     -> fail $ "if-else fail"
        While expr stmt		-> do
            end_label_id <- getLabel
            loop_label_id <- getLabel
            then_label_id <- getLabel
            let end_label = "lab" ++ (show end_label_id)
            let loop_label = "lab" ++ (show loop_label_id)
            let then_label = "lab" ++ (show then_label_id)
            
            tmp_val_reg <- newRegister (Ident "tmp") False
            tobool_reg <- newRegister (Ident "tobool") False
            
            --  Load Type Register Register -- Load type a b (%a = load type %b)
            putInstruction $ AddLit Plus Bool tmp_val_reg "0" "0"
            putInstruction $ BrUnCond loop_label
            putInstruction $ Label loop_label Nop
            
            (val,typ) <- compileExp expr
            
            case val of
                Just reg -> do

                    -- putInstruction $ Load typ
                    -- need to load the value of the expressions
                    putInstruction $ IfCmp NE typ tobool_reg reg tmp_val_reg
                    putInstruction $ BrCond tobool_reg then_label end_label
                    putInstruction $ Label then_label Nop
                    compileStm stmt
                    
                    putInstruction $ BrUnCond loop_label
                    
                    putInstruction $ Label end_label Nop
                    
                Nothing -> fail $ "while-stm fail"
			
            {-label_id_1 <- getLabel
			label_id_2 <- getLabel
			let label_1 = "lab" ++ (show label_id_1)
			let label_2 = "lab" ++ (show label_id_2)

			putInstruction $ Goto label_2
			putInstruction $ Label label_1
			compileStm stmt
			putInstruction $ Label label_2
			compileExp expr	
			putInstruction $ IfNe label_1 -}
            
        SExp exprs		-> do
            compileExp exprs
            return ()
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
		     
		Incr name		DONE
		Decr name		DONE
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
	
	params <- mapM registerArgs args
	
	putInstruction $ FunctionBegin name params retType
	mapM_ (compileStm) stms
	putInstruction $ FunctionEnd
	
	code_stack <- gets codeStack
	prog_code <- gets programCode
	modify (\e -> e { programCode = prog_code ++ [code_stack] })
	
	where
		registerArgs :: Arg -> CP (Register, Type)
		registerArgs (Arg t ident) = do
			reg <- addVarNonPointer t ident
			return (reg, t)
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
typeToLLVMType Bool = "i1"
typeToLLVMType Void = "void"

transLLVMInstr :: LLVMInstruction -> String
transLLVMInstr instr = do
    case instr of
        FunctionBegin name p rettype -> "define " ++ typeToLLVMType(rettype) ++ " @" ++ name ++ "(" ++ transRegList(p) ++ ") {\nentry:"
        FunctionEnd                  -> "}\n"
        Return t reg                 -> "\tret " ++ typeToLLVMType(t) ++ transRegName(reg)
        ReturnLit t lit              -> "\tret " ++ typeToLLVMType(t) ++ " " ++ lit
        ReturnVoid                   -> "\tret void"
        Alloc t (reg, _)             -> "\t" ++ reg ++ " = alloca " ++ typeToLLVMType(t)
        StoreLit t v reg             -> "\tstore " ++ typeToLLVMType(t) ++ " " ++ v ++ ", " ++ typeToLLVMType(t) ++ transRegName(reg)
        Store t reg1 reg2            -> "\tstore " ++ typeToLLVMType(t) ++ transRegName(reg1) ++ ", " ++ typeToLLVMType(t) ++ transRegName(reg2)
        Load t (reg1, _) reg2        -> "\t" ++ reg1 ++ " = load " ++ typeToLLVMType(t) ++ transRegName(reg2)   -- Load type a b (%a = load type %b)
        Add op t reg1 reg2 val          -> "\t" ++ transRegName(reg1) ++ " = " ++ transAddOp(op) ++ " " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ val
        AddRegs op t reg1 reg2 reg3     -> "\t" ++ transRegName(reg1) ++ " = " ++ transAddOp(op) ++ " " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
        AddLit op t (reg, _) val1 val2  -> "\t" ++ reg ++ " = " ++ transAddOp(op) ++ " " ++ typeToLLVMType(t) ++ " " ++ val1 ++ ", " ++ val2
        --ICmpNe t reg1 reg2 reg3      -> "\t" ++ transRegName(reg1) ++ " = icmp ne " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
        IfCmp op t reg1 reg2 reg3    -> do
            case op of
                NE  -> "\t" ++ transRegName(reg1) ++ " = icmp ne " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                EQU  -> "\t" ++ transRegName(reg1) ++ " = icmp eq " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                GTH  -> "\t" ++ transRegName(reg1) ++ " = icmp sgt " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                LTH  -> "\t" ++ transRegName(reg1) ++ " = icmp slt " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                GE  -> "\t" ++ transRegName(reg1) ++ " = icmp sge " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                LE  -> "\t" ++ transRegName(reg1) ++ " = icmp sle " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)

        BrCond (reg,_) lab_t lab_f   -> "\t" ++ "br i1 " ++ reg ++ ", label %" ++ lab_t ++ ", label %" ++ lab_f
        BrUnCond label               -> "\t" ++ "br label %" ++ label
        Label lbl instr              -> lbl ++ ": " ++ transLLVMInstr(instr)
        FuncCall (Ident n) t rs out_r  -> do
            case t of
                Void      -> "\tcall " ++ typeToLLVMType(t) ++ " @" ++ n ++ "(" ++ transRegList(rs) ++ ")"
                otherwise -> "\t" ++ transRegName(out_r) ++ " = call " ++ typeToLLVMType(t) ++ " @" ++ n ++ "(" ++ transRegList(rs) ++ ")"
        Negation t reg1 reg2           -> "\t" ++ transRegName(reg1) ++ " = xor " ++ typeToLLVMType(t) ++ " " ++ transRegName(reg2) ++ ", 1"
        Mul t reg1 reg2 reg3         -> "\t" ++ transRegName(reg1) ++ " = mul " ++ typeToLLVMType(t) ++ " " ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
        Modulus t reg1 reg2 reg3         -> "\t" ++ transRegName(reg1) ++ " = srem " ++ typeToLLVMType(t) ++ " " ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
        Division t reg1 reg2 reg3         -> "\t" ++ transRegName(reg1) ++ " = sdiv " ++ typeToLLVMType(t) ++ " " ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
        --Add Type String String String -- Add type to_reg from_reg value
        otherwise -> fail $ "Trying to translate unknown instruction!"
	where
		-- translate a parameter list in a function
		{-transParlist :: [(Register, Type)] -> String
		transParlist []   = ""
		transParlist p@((r, ptr):[]) = typeToLLVMType(t) ++ " %" ++ n
		transParlist p@((r, ptr):ps) = typeToLLVMType(t) ++ " %" ++ n ++ ", " ++ transParlist(ps)-}
		
		transRegList :: [(Register, Type)] -> String
		transRegList []   = ""
		transRegList (((r, _), t):[]) = typeToLLVMType(t) ++ " " ++ r
		transRegList (((r, _), t):rs) = typeToLLVMType(t) ++ " " ++ r ++ ", " ++ transRegList(rs)

		-- translate register name
		transRegName :: Register -> String
		transRegName (reg_name, True)  = "* " ++ reg_name
		transRegName (reg_name, False) = " " ++ reg_name
	
		-- translate add operator
		transAddOp :: AddOp -> String
		transAddOp Plus = "add"
		transAddOp Minus = "sub"


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
	mapM addDef defs
	
	-- compile all function defines
	mapM compileDef defs
	
	-- translate jasmine code to strings
	pgm_code <- gets programCode
	--fail $ show pgm_code
	mapM_ (transLLVMBlock) pgm_code
	
	-- compiledCode now has translated jasmine values
	compiled_code <- gets compiledCode
	return $ compiled_code

