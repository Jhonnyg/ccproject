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

data JasminInstr = Push 

-- Replace [(from,to)]
data Env = Env { signatures :: Map Ident Type,
		 nextVarIndex :: Integer,
		 nextLabelIndex :: Integer,
		 currentStackDepth :: Integer,
		 maxStackDepth :: Integer,
		 codeStack :: [JasminInstr] }


{- -- all the type signatures from all the functions
                      signatures :: Map Ident ([Type], Type)
                    , contexts :: [Map Ident Type]
		    , returnType :: Type -}

stdFuncs = [(Ident "printInt", ([Int],Void)),
	    (Ident "readInt", ([],Int)),
	    (Ident "printDouble", ([Doub],Void)),
	    (Ident "readDouble", ([],Doub))]	

-- Create an empty environment
emptyEnv :: Env
emptyEnv = Env { signatures = Map.fromList stdFuncs
               , contexts = [Map.empty]
	       , returnType = undefined }

compile :: Program -> Err ()
compile p = (evalStateT . unCPM) (checkTree p) emptyEnv

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
inferExp :: Expr -> CP Type
inferExp expr = do
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
		Not expr		-> undefined
		EMul e0 op e1		-> undefined
		EAdd e0 op e1		-> undefined
		ERel e0 (EQU) e1 	-> undefined
		ERel e0 op e1		-> undefined
		EAnd e0 e1		-> undefined
		EOr e0 e1		-> undefined
		
compileStm :: Stmt -> CP ()
compileStm stm = do
	case stm of
		SType t stmt 		-> undefined
		Empty 			-> undefined
		BStmt (Block stmts) 	-> undefined
			
		Decl  t itmList		-> undefined
		  			
		Ass name epxr		-> undefined
		     
		Incr name		-> undefined
		   
		Decr name		-> undefined
		   
		Ret  expr     		-> undefined
		 
		VRet     		-> undefined
		   
		Cond expr stmt		-> undefined
		   
		CondElse  expr ifs els  -> undefined
		 
		While expr stmt		-> undefined
  		 
		SExp exprs		-> undefined
		
compileDef :: TopDef -> CP ()
compileDef (FnDef retType name args (Block stms)) = undefined

{-	pushContext
	modify (\e -> e { returnType = retType } )
	mapM_ addArgs args  
	mapM checkStm stms
	popContext
	return()
	where
	addArgs :: Arg -> TC ()
	addArgs (Arg t i) = addVar i t
-}

checkTree (Program defs) = do
	--mapM compileDef defs
	--mapM compileDef defs
	return ()
