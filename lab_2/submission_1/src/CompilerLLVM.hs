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
	| FuncCallString String String
    | Mul Type Register Register Register
    | Modulus Type Register Register Register
    | Division Type Register Register Register
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
		 variables :: [Map Ident (Register, Type)],
		 registers :: Map Ident Int,
		 string_constants :: [String],
		 next_string_num :: Integer,
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

-- add variable to current context
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
                string_constants = [],
                next_string_num = 0,
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
        EAppS _ str -> do
            -- add to string constants
            let str' = str ++ "\00"
            string_num <- gets next_string_num
            const_strings <- gets string_constants
            let const_name = "@.str" ++ (show string_num)
            let string_constant = const_name ++ " = private constant [" ++ (show $ length(str')) ++ " x i8] c\"" ++ str' ++ "\""
            
            modify (\e -> e { next_string_num = string_num, string_constants = string_constant:const_strings } )
            
            -- push instruction
            putInstruction $ FuncCallString str' const_name
            
            return (Nothing, Void)
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
            if t == Doub
                then putInstruction $ AddLit Plus Doub lit_reg "0.0" "-1.0"
                else putInstruction $ AddLit Plus Int lit_reg "0" "-1"
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
            (Just reg0,t) <- compileExp e0
            (Just reg1,_) <- compileExp e1
            
            -- labels
            true_label_id <- getLabel
            false_label_id <- getLabel
            out_label_id <- getLabel
            let true_label = "lab" ++ (show true_label_id)
            let false_label = "lab" ++ (show false_label_id)
            let out_label = "lab" ++ (show out_label_id)
            
            -- registers
            res_reg1 <- newRegister (Ident "res") False
            ret_reg_ptr <- newRegister (Ident "ret") True -- store in memory
            ret_reg <- newRegister (Ident "ret") False
            
            -- alloc return value in memory, and branch on first expression result
            putInstruction $ Alloc Bool ret_reg_ptr
            putInstruction $ BrCond reg0 true_label false_label
            
            -- first expression is true, store result of second expression
            putInstruction $ Label true_label Nop
            putInstruction $ Store Bool reg1 ret_reg_ptr
            putInstruction $ BrUnCond out_label
            
            -- first expression is false, store result of first expression (since it should be false)
            putInstruction $ Label false_label Nop
            putInstruction $ Store Bool reg0 ret_reg_ptr
            putInstruction $ BrUnCond out_label
            
            -- load return value from memory and return value in register
            putInstruction $ Label out_label Nop
            putInstruction $ Load Bool ret_reg ret_reg_ptr
            
            return (Just ret_reg, Bool)
            
        EOr e0 e1 -> do
            (Just reg0,t) <- compileExp e0
            (Just reg1,_) <- compileExp e1
            
            -- labels
            true_label_id <- getLabel
            false_label_id <- getLabel
            out_label_id <- getLabel
            let true_label = "lab" ++ (show true_label_id)
            let false_label = "lab" ++ (show false_label_id)
            let out_label = "lab" ++ (show out_label_id)
            
            -- registers
            res_reg1 <- newRegister (Ident "res") False
            ret_reg_ptr <- newRegister (Ident "ret") True -- store in memory
            ret_reg <- newRegister (Ident "ret") False
            
            -- alloc return value in memory, and branch on first expression result
            putInstruction $ Alloc Bool ret_reg_ptr
            putInstruction $ BrCond reg0 true_label false_label
            
            -- first expression is true, store result of first expression (we dont need to know the second one)
            putInstruction $ Label true_label Nop
            putInstruction $ Store Bool reg0 ret_reg_ptr
            putInstruction $ BrUnCond out_label
            
            -- first expression is false, store result of second expression (we need to know the second one)
            putInstruction $ Label false_label Nop
            putInstruction $ Store Bool reg1 ret_reg_ptr
            putInstruction $ BrUnCond out_label
            
            -- load return value from memory and return value in register
            putInstruction $ Label out_label Nop
            putInstruction $ Load Bool ret_reg ret_reg_ptr
            
            return (Just ret_reg, Bool)
            
	where
		tidyRegs :: [(Maybe Register, Type)] -> [(Register, Type)]
		tidyRegs [] = []
		tidyRegs ((Just r, t):rs) = (r, t):tidyRegs(rs)

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
        (Just reg_from, t') <- compileExp expr
        putInstruction $ (Store t reg_from reg_name)

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
            (Just reg_val,typ) <- compileExp expr
            putInstruction $ Return typ reg_val 
        
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
            (Just reg_val,exp_typ) <- compileExp expr
            (reg,var_typ) <- getVar name
            putInstruction $ Store var_typ reg_val reg
            
        Cond expr stmt		-> do
            end_label_id <- getLabel
            then_label_id <- getLabel
            let end_label = "lab" ++ (show end_label_id)
            let then_label = "lab" ++ (show then_label_id)
            
            case expr of
                ELitTrue -> compileStm stmt
                otherwise -> do
                    (Just reg@(name,ptr),typ) <- compileExp expr
                    
                    tmp_val_reg <- newRegister (Ident "tmp") False
                    tobool_reg <- newRegister (Ident "tmp") False
                    if typ == Doub
                        then putInstruction $ AddLit Plus Doub tmp_val_reg "0.0" "0.0"
                        else putInstruction $ AddLit Plus Bool tmp_val_reg "0" "0"
                    putInstruction $ IfCmp NE typ tobool_reg reg tmp_val_reg -- save result to tobool_reg
                    putInstruction $ BrCond tobool_reg then_label end_label
                    putInstruction $ Label then_label Nop
                    compileStm stmt                    
                    putInstruction $ BrUnCond end_label
                    putInstruction $ Label end_label Nop
            
        CondElse  expr ifs els  -> do
            end_label_id <- getLabel
            then_label_id <- getLabel
            else_label_id <- getLabel
            let end_label = "lab" ++ (show end_label_id)
            let then_label = "lab" ++ (show then_label_id)
            let else_label = "lab" ++ (show else_label_id)
            (Just reg@(_,ptr),typ) <- compileExp expr
            
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

        While expr stmt		-> do
            -- labels
            end_label_id <- getLabel
            loop_label_id <- getLabel
            then_label_id <- getLabel
            let end_label = "lab" ++ (show end_label_id)
            let loop_label = "lab" ++ (show loop_label_id)
            let then_label = "lab" ++ (show then_label_id)
            
            tmp_val_reg <- newRegister (Ident "tmp") False
            tobool_reg <- newRegister (Ident "tobool") False
            
            putInstruction $ AddLit Plus Bool tmp_val_reg "0" "0"
            putInstruction $ BrUnCond loop_label
            putInstruction $ Label loop_label Nop
            
            (Just reg,typ) <- compileExp expr

            putInstruction $ IfCmp NE typ tobool_reg reg tmp_val_reg
            putInstruction $ BrCond tobool_reg then_label end_label
            putInstruction $ Label then_label Nop
            compileStm stmt
            
            putInstruction $ BrUnCond loop_label
            
            putInstruction $ Label end_label Nop
                    
            
        SExp exprs		-> do
            compileExp exprs
            return ()
        unknown -> fail $ "Trying to compile an unknown statement! " ++ (show stm)

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
        AddLit op t@Doub (reg, _) val1 val2  -> "\t" ++ reg ++ " = " ++ transAddOpD(op) ++ " " ++ typeToLLVMType(t) ++ " " ++ val1 ++ ", " ++ val2
        AddLit op t (reg, _) val1 val2  -> "\t" ++ reg ++ " = " ++ transAddOp(op) ++ " " ++ typeToLLVMType(t) ++ " " ++ val1 ++ ", " ++ val2
    
        IfCmp op t@(Doub) reg1 reg2 reg3    -> do
            case op of
                NE  -> "\t" ++ transRegName(reg1) ++ " = fcmp ne " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                EQU  -> "\t" ++ transRegName(reg1) ++ " = fcmp eq " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                GTH  -> "\t" ++ transRegName(reg1) ++ " = fcmp sgt " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                LTH  -> "\t" ++ transRegName(reg1) ++ " = fcmp slt " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                GE  -> "\t" ++ transRegName(reg1) ++ " = fcmp sge " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
                LE  -> "\t" ++ transRegName(reg1) ++ " = fcmp sle " ++ typeToLLVMType(t) ++ transRegName(reg2) ++ ", " ++ transRegName(reg3)
            
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
        FuncCallString str constr -> "\tcall void @printString(i8* getelementptr inbounds ([ " ++ (show $ length(str)) ++ " x i8 ]* " ++ constr ++ ", i32 0, i32 0))"
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
        
        -- translate add operator Doubles
		transAddOpD :: AddOp -> String
		transAddOpD Plus = "fadd"
		transAddOpD Minus = "fsub"


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
	
	-- add constants
	constants <- gets string_constants
	let constants' = intersperse "\n" constants
	let compiled_code' = constants' ++ compiled_code
	
	return $ compiled_code'

