{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Data.Maybe (fromJust, catMaybes, isNothing)

import Data.Map (Map)
import qualified Data.Map as Map

-- Encapsulate environment in a state monad as to enable error and state handling at the same time 
newtype TCM m a = TCM { unTCM :: StateT Env m a }
    deriving (Monad, MonadTrans, MonadState Env)

-- Type alias to increase readability
type TC a = TCM Err a

-- Replace [(from,to)]
data Env = Env { -- all the type signatures from all the functions
                      signatures :: Map CIdent ([Type], Type)
                 -- a stack of contexts, one per scope 
                 -- this is NEVER empty. if empty then it always should contain
                 -- Map.empty (making lookups never crash)
                    , contexts :: [Map CIdent Type]
		-- local return type for current function definition 
		    , returnType :: Type }

-- Create an empty environment
emptyEnv :: Env
emptyEnv = Env { signatures = Map.empty
               , contexts = [Map.empty]
	       , returnType = undefined }

typecheck :: Program -> Err ()
typecheck p = (evalStateT . unTCM) (checkTree p) emptyEnv

-- place one empty context at the top of the stack
pushContext :: TC ()
pushContext = modify (\e -> e { contexts = (Map.empty :  contexts e)})

-- remove topmost context from the stack
popContext :: TC ()
popContext = do
        ctxStack <- gets contexts
        when (null ctxStack) (error "popping from an empty context stack")
        modify (\e -> e { contexts = tail ctxStack })

-- add a variable to current context and fail if it already exists
addVar :: CIdent -> Type -> TC ()
addVar n t = do
        env <- get
        let (c:cs) = contexts env
        when (Map.member n c) $ fail $ "adding a variable " ++ (show n) ++ " that is already in top context"
        let c' = Map.insert n t c
        let env' = env { contexts = (c' : cs) }
        put env'

-- add a function definition
addDef :: Def -> TC ()
addDef (FunctionDef retType n as _) = do
	sigs <- gets signatures 
	let ts = map argToType as
	let sigs' = Map.insert n (ts,retType) sigs
	modify (\e -> e { signatures = sigs' } ) -- updates the state record signatures
	where 
	argToType :: Arg -> Type
	argToType (Arg t _) = t
	
-- Look for a variable in allall the contextss
lookVar :: CIdent -> TC Type
lookVar n = do
        ctxs <- gets contexts 
        rtrn (catMaybes ((map (Map.lookup n) ctxs)))
        where
	-- if we cant find anything, make sure we fail
        rtrn :: [Type] -> TC Type
        rtrn [] = fail $ "type of " ++ (show n) ++ " not found"
        rtrn (x:xs) = return x

-- Look for a function in the signatures
lookFun :: CIdent -> TC ([Type], Type)
lookFun fName = do
	mbtSig <- gets (Map.lookup fName. signatures)
	when (isNothing mbtSig) (fail $ "Unknown function name")
	return $ fromJust mbtSig
	

inferExp :: Exp -> TC Type
inferExp expr = do
	case expr of
		EDouble _           -> return TDouble
		EInt _              -> return TInt
		EBoolean _          -> return TBool
		EIdent name         -> lookVar name 
		EFunCall fName args -> do  
				(argList,retType)<- lookFun fName --([Type],Type)
				inferredTypes <- mapM inferExp args -- [Type] 
				if argList == inferredTypes 
					then return TBool 
					else fail $ "Arguments does not match: " ++ (show inferredTypes) ++ "," ++ (show argList)
				return retType 
		EPostInc exp        -> checkUnaryOperation exp
		EPostDec exp        -> checkUnaryOperation exp
		EPreInc  exp        -> checkUnaryOperation exp
		EPreDec  exp        -> checkUnaryOperation exp
		EMul e0 e1          -> checkBinaryOperation e0 e1
		EDiv e0 e1          -> checkBinaryOperation e0 e1
		EPlus e0 e1         -> checkBinaryOperation e0 e1
		EMinus e0 e1        -> checkBinaryOperation e0 e1
		EGT e0 e1           -> checkComparator e0 e1
		ELT e0 e1           -> checkComparator e0 e1 
		ELEq e0 e1          -> checkComparator e0 e1
		EGEq e0 e1          -> checkComparator e0 e1
		EEqual e0 e1        -> checkComparator e0 e1
		ENEqual e0 e1       -> checkComparator e0 e1
		ELAnd e0 e1         -> checkBoolean e0 e1 
		ELOr e0 e1          -> checkBoolean e0 e1
		EAss e0 e1          -> checkBinaryOperation e0 e1  

-- Check unary numeric operations such as ++ (exp)
checkUnaryOperation :: Exp -> TC Type
checkUnaryOperation exp = do
		iType <- inferExp exp
		if elem iType [TInt, TDouble] then return iType else fail $ (show iType) ++ "invalid expression" 

-- Check binary numeric operations 
checkBinaryOperation :: Exp -> Exp -> TC Type
checkBinaryOperation e0 e1 = do 
		iType0 <- inferExp e0
		iType1 <- inferExp e1
		if (iType0 == iType1 && iType0 /= TBool && iType0 /= TVoid)
			then return iType0
			else fail $ "Arithmetic operation have different argument types: " 	++ (show iType0) ++ "," ++ (show iType1) 

checkComparator :: Exp -> Exp -> TC Type
checkComparator e0 e1 = do
		iType0 <- inferExp e0
		iType1 <- inferExp e1
		if iType0 == iType1
			then return TBool
			else fail $ "Cannot compare " ++ (show iType0) ++ " with " ++ (show iType1)
		
checkBoolean :: Exp -> Exp -> TC Type
checkBoolean e0 e1 = do
		iType0 <- inferExp e0
		iType1 <- inferExp e1
		if iType0 == TBool && iType1 == TBool
			then return TBool
			else fail $ "Boolean operation has different argument types: " ++ (show iType0) ++ "," ++ (show iType1)

checkStm :: Stm -> TC ()
checkStm stm = do
	case stm of
		SDeclaration (ArgumentDecl t idList) -> do
						mapM_ ((flip  addVar) t) idList
		SDeclaration (ArgumentDecl2 t n expr)-> do
						addVar n t
						exprType <- inferExp expr
						when (exprType /= t) (fail $ "Declaration type does not match: " ++ (show t) ++ "," ++ (show exprType))
						return ()
		SBlock stmList -> do
				pushContext
				mapM_ checkStm stmList
				popContext
		SWhile expr stm -> do
				exprType <- inferExp expr
				when (exprType /= TBool) (fail $ "While condition " ++ (show expr) ++ " is not Bool")
				checkStm stm 
				return ()
		SIfElse expr lhs rhs -> do 
				exprType <- inferExp expr
				when (exprType /= TBool) (fail $ "Expression passed to while statement does not evaluate to Bool")
				checkStm lhs
				checkStm rhs
		SReturn expr -> do 
				expType <- inferExp expr
				retType <- gets returnType
				if (expType == retType) then return () else fail "return type error"
		SExp expr -> do
				inferExp expr
				return()


checkDef :: Def -> TC ()
checkDef (FunctionDef retType name args stms) = do
	pushContext
	modify (\e -> e { returnType = retType } )
	mapM_ addArgs args  
	mapM checkStm stms
	popContext
	return()
	where
	addArgs :: Arg -> TC ()
	addArgs (Arg t i) = addVar i t

checkTree (Program defs) = do
	mapM addDef defs
	mapM checkDef defs
	return ()
