{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Environment
import Control.Monad.Except
import Control.Applicative
import Control.Monad.State
import Control.Monad.Identity
import LLVM.Module
import LLVM.Context
import LLVM.AST.Type
import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import Data.ByteString (ByteString, pack)
import Data.String.Transform (toShortByteString)
import qualified Data.Text as DT
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
-- CONSTANTS
--------------------------------------------------------

memSize :: Integer
memSize = 65536

memPos :: Integer
memPos = floor $ (realToFrac memSize) / 2

--------------------------------------------------------
-- UTIL
--------------------------------------------------------

unnl :: String -> String
unnl s = DT.unpack $ DT.replace (DT.pack "\\n") (DT.pack ['\n']) (DT.pack s)

bsToS s = unnl $ show s

sToBs s = toShortByteString s

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
-- LLVM TYPES
--------------------------------------------------------

memTy :: Type
memTy = ArrayType (fromInteger memSize) i8

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
    putStrLn $ bsToS llstr
    return ast
  where mod = codegenTop ts
        ast = runLLVM mod

codegenTop :: [Token] -> LLVM ()
codegenTop ts = do
  defVar "MEM" memTy $ Just $ C.AggregateZero memTy
  defVar "MEM_POS" i32 $ Just $ C.Int 32 memPos
  addDef $ TypeDefinition (Name "FILE") $ Nothing
  defExternal "fflush" i32 [(NamedTypeReference (Name "FILE"), Name "")]
  defExternal "putchar" i32 [(i8, Name "")]
  defExternal "getchar" i32 []
  defMain []

addDef :: Definition -> LLVM ()
addDef d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

defMain :: [BasicBlock] -> LLVM ()
defMain body = addDef $
  GlobalDefinition $ functionDefaults
  { name = Name "main"
  , parameters = ([], False)
  , returnType = i32
  , basicBlocks = body
  }

defExternal :: String -> Type -> [(Type, Name)] -> LLVM ()
defExternal n t args = addDef $
  GlobalDefinition $ functionDefaults
  { name = Name $ sToBs n
  , linkage = L.External
  , parameters = ([Parameter ty nm [] | (ty, nm) <- args], False)
  , returnType = t
  , basicBlocks = []
  }

defVar :: String -> Type -> Maybe C.Constant -> LLVM ()
defVar n t i = addDef $
  GlobalDefinition $ globalVariableDefaults
  { name = Name $ sToBs n
  , LLVM.AST.Global.type' = t
  , initializer = i
  }

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
