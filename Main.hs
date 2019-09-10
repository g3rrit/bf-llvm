{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Environment
import Control.Monad.Except
import Control.Applicative
import Control.Monad.State
import Control.Monad.Identity
import LLVM.Module
import LLVM.Context
import qualified LLVM.AST as AST

type BfError = String

data Token = RGT -- (>) increment memory pointer
           | LFT -- (<) decrement memory pointer
           | INC -- (+) increment current cell
           | DEC -- (-) decrement current cell
           | PRT -- (.) print value of current cell
           | GET -- (,) read value into current cell
           | LOL -- ([) jump to next block, if current cell == 0
           | LOR -- (]) jump to previous block, if current cell != 0
           deriving Show

--------------------------------------------------------
-- TOKENIZER
--------------------------------------------------------

tokenize :: String -> [Token]
tokenize [] = []
tokenize (s:ss) =
  case s of
    '>' -> RGT : (tokenize ss)
    '<' -> LFT : (tokenize ss)
    '+' -> INC : (tokenize ss)
    '-' -> DEC : (tokenize ss)
    '.' -> PRT : (tokenize ss)
    ',' -> GET : (tokenize ss)
    '[' -> LOL : (tokenize ss)
    ']' -> LOR : (tokenize ss)
    _ -> tokenize ss


--------------------------------------------------------
-- LLVM CODEGEN
--------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

initModule :: AST.Module
initModule = AST.defaultModule { AST.moduleName = "bf" }

runLLVM :: LLVM a -> AST.Module
runLLVM (LLVM m) = execState m initModule

codegen :: [Token] -> IO AST.Module
codegen ts = withContext $ \context ->
  withModuleFromAST context ast $ \m -> do
    llstr <- moduleLLVMAssembly m
    print llstr
    return ast
  where mod = codegenTop ts
        ast = runLLVM mod

codegenTop :: [Token] -> LLVM ()
codegenTop = \s -> return ()

--------------------------------------------------------
-- MAIN ROUTINE
--------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Brainfuck llvm compiler"
  args <- getArgs
  let path = head args
  putStrLn $ "File: " ++ path
  content <- readFile path
  putStrLn $ "Content: " ++ content
  putStrLn $ "Token: "
  let ts = tokenize content
  print ts

  codegen ts
  return ()
