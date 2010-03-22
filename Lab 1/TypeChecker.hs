{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeChecker where

import Absjavalette
import Printjavalette
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
                      signatures :: Map Ident ([Type], Type)
                    , contexts :: [Map Ident Type]
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
	

inferExp :: Expr -> TC Type
inferExp expr = do
	case expr of
		EVar name 	-> do
			varName <- lookVar name
			return varName
		ELitInt i 	-> return Int 
		ELitDoub d 	-> return Doub
		ELitTrue	-> return Bool
		ELitFalse	-> return Bool
		EApp n expList 	-> undefined
		EAppS n str	-> undefined
		Neg expr	-> undefined
		Not expr	-> undefined
		EMul e0 op e1	-> undefined
		EAdd e0 op e1	-> undefined
		ERel e0 op e1	-> undefined
		EAnd e0 e1	-> checkBoolean e0 e1
		EOr e0 e1	-> checkBoolean e0 e1
		

-- Check unary numeric operations such as ++ (exp)
checkUnaryOperation :: Expr -> TC Type
checkUnaryOperation exp = do
		iType <- inferExp exp
		if elem iType [Int, Doub] 
			then return iType 
			else fail $ (show iType) ++ "invalid expression" 

-- Check binary numeric operations 
checkBinaryOperation :: Expr -> Expr -> TC Type
checkBinaryOperation e0 e1 = do 
		iType0 <- inferExp e0
		iType1 <- inferExp e1
		if (iType0 == iType1 && iType0 /= Bool && iType0 /= Void)
			then return iType0
			else fail $ "Arithmetic operation have different argument types: " ++ (show iType0) ++ "," ++ (show iType1) 

checkComparator :: Expr -> Expr -> TC Type
checkComparator e0 e1 = do
		iType0 <- inferExp e0
		iType1 <- inferExp e1
		if iType0 == iType1
			then return Bool
			else fail $ "Cannot compare " ++ (show iType0) ++ " with " ++ (show iType1)
		
checkBoolean :: Expr -> Expr -> TC Type
checkBoolean e0 e1 = do
		iType0 <- inferExp e0
		iType1 <- inferExp e1
		if iType0 == Bool && iType1 == Bool
			then return Bool
			else fail $ "Boolean operation has different argument types: " ++ (show iType0) ++ "," ++ (show iType1)

checkStm :: Stmt -> TC ()
checkStm stm = do
	case stm of
		Empty 			-> undefined
		BStmt (Block stmts) 	-> undefined
		Decl  t itmList		-> undefined
		--NoInit name		-> undefined i think this is used in interpreter to flag wheter or not a variable is intiated with a value or not!
		--Init name expr	-> undefined
		Ass name epxr		-> undefined
		Incr name		-> undefined
		Decr name		-> undefined
		Ret  expr     		-> do
		  inferExp expr
		  return ()
		VRet     		-> do
		  rettype <- gets returnType
		  if rettype == Void
		    then return ()
		    else fail $ "Trying to return void in a function of type: " ++ (show rettype)
		    
		Cond expr stmt		-> do
		  exptype <- inferExp expr
		  when (exptype /= Bool) (fail $ "Conditional expression for if-else-statement  not of boolean type: " ++ (show exptype))
		  checkStm stmt
		  return ()
		    
		CondElse  expr ifs els  -> do
		  exptype <- inferExp expr
		  when (exptype /= Bool) (fail $ "Conditional expression for if-statement not of boolean type: " ++ (show exptype))
		  checkStm ifs
		  checkStm els
		  return ()
		  
		While expr stmt		->   do
  		  exptype <- inferExp expr
  		  when (exptype /= Bool) (fail $ "Conditional expression for while-statement not of boolean type: " ++ (show exptype))
  		  checkStm stmt
  		  return ()
  		  
		SExp exprs		-> do
		  inferExp exprs
		  return ()
		

checkDef :: TopDef -> TC ()
checkDef (FnDef retType name args (Block stms)) = do
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
