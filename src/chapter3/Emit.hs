{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.Module (withModuleFromAST, moduleLLVMAssembly)
import LLVM.Context (withContext)

import qualified LLVM.AST.Global as AST (name, returnType)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Control.Monad.State
import Control.Monad.Except
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Char8 as BS (putStrLn)
import qualified Data.Map as Map

import Codegen
import qualified Syntax as S

toSig :: [S.Name] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.mkName x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  defs <- gets AST.moduleDefinitions
  let bls = createBlocks $ execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry
              forM args $ \a -> do
                var <- alloca double
                store var (local (AST.mkName a))
                assign a var
              cgen (lookupFnTypeH defs) body >>= ret
  define double (AST.mkName name) fnargs bls
  where
    fnargs = toSig args

codegenTop (S.Extern name args) = do
  external double (AST.mkName name) fnargs
  where fnargs = toSig args

codegenTop exp = do
  defs <- gets AST.moduleDefinitions
  let blks = createBlocks $ execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry
              cgen (lookupFnTypeH defs) exp >>= ret
  define double "main" [] blks


-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: (AST.Name -> AST.Type) -> S.Expr -> Codegen AST.Operand
cgen lookupFnType (S.UnaryOp op a) = do
  cgen lookupFnType $ S.Call ("unary" ++ op) [a]
cgen lookupFnType (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen lookupFnType val
  store a cval
  return cval
cgen lookupFnType (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen lookupFnType a
      cb <- cgen lookupFnType b
      f ca cb
    Nothing -> error "No such operator"
cgen _ (S.Var x) = getvar x >>= load
cgen _ (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen lookupFnType (S.Call fn args) = do
  largs <- mapM (cgen lookupFnType) args
  call (externf fnType fnnm) largs
  where
    fnnm = AST.mkName fn
    fnType = lookupFnType fnnm

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    BS.putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
