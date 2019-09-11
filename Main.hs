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
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Typed as T
import Data.ByteString (ByteString, pack)
-- import qualified Data.ByteString.Short as S
-- import qualified Data.ByteString.Char8 as B
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
memSize = 1048576

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

ptrTy :: Type -> Type
ptrTy t = PointerType t $ AS.AddrSpace 0

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
  defExternal "fflush" i32 [(ptrTy $ NamedTypeReference (Name "FILE"), Name "")]
  defExternal "putchar" i32 [(i8, Name "")]
  defExternal "getchar" i32 []
  defMain $ bbCodegen ts

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
-- BASIC BLOCK CODEGEN
--------------------------------------------------------

data BlockState = BlockState
  { idx   :: Int -- block id
  , label :: Name
  , ins   :: [Named Instruction]
  , ter   :: Maybe (Named Terminator)
  }

data CodegenState = CodegenState
  { bid    :: Int   -- id count
  , lstack :: [Int] -- block id stack
  , count  :: Word  -- instruction count
  , blocks :: [BlockState]
  }

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

blocksToBb :: [BlockState] -> [BasicBlock]
blocksToBb bs = reverse $ map (\(BlockState _ l i (Just t)) -> BasicBlock l i t) bs

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) $ CodegenState 0 [] 0 [BlockState 0 (Name "entry") [] Nothing]

bbCodegen :: [Token] -> [BasicBlock]
bbCodegen ts = blocksToBb $ blocks $ execCodegen $ (mapM transToken ts) >> ret

transToken :: Token -> Codegen ()
transToken t = case t of
                 RGT -> rgt
                 LFT -> lft
                 INC -> inc
                 DEC -> dec
                 PRT -> prt
                 GET -> gte
                 LOL -> lol
                 LOR -> lor

rgt :: Codegen ()
rgt = do
  m0 <- load memPtr
  m1 <- add m0 c1i32
  store m1 memPtr

lft :: Codegen ()
lft = do
  m0 <- load memPtr
  m1 <- sub m0 c1i32
  store m1 memPtr

inc :: Codegen ()
inc = do
  m0 <- load memPtr
  m1 <- getElementPtr mem m0
  m2 <- load m1
  m3 <- add m2 c1i8
  store m3 m1

dec :: Codegen ()
dec = do
  m0 <- load memPtr
  m1 <- getElementPtr mem m0
  m2 <- load m1
  m3 <- sub m2 c1i8
  store m3 m1

prt :: Codegen ()
prt = do
  m0 <- load memPtr
  m1 <- getElementPtr mem m0
  m2 <- load m1
  call fnPutchar [m2]
  call fnFflush [ConstantOperand $ C.Null $ ptrTy $ NamedTypeReference $ Name "FILE"]
  return ()

gte :: Codegen ()
gte = do
  m0 <- call fnGetchar []
  m1 <- trunc m0 i8
  m2 <- load memPtr
  m3 <- getElementPtr mem m2
  store m1 m3

lol :: Codegen ()
lol = do
  pushBid
  loop
  i <- getBid
  pushBlock $ BlockState i (Name $ sToBs $ "l." ++ (show i)) [] Nothing

lor :: Codegen ()
lor = do
  loop
  i <- popBid
  pushBlock $ BlockState i (Name $ sToBs $ "r." ++ (show i)) [] Nothing

loop :: Codegen ()
loop = do
  m0 <- load memPtr
  m1 <- getElementPtr mem m0
  m2 <- load m1
  m3 <- icmp m2 c0i8
  i <- getBid
  cbr m3 (Name $ sToBs $ "r." ++ (show i)) (Name $ sToBs $ "l." ++ (show i))

pushBlock :: BlockState -> Codegen ()
pushBlock b = modify (\(CodegenState i is c bs) ->
                          CodegenState i is c (b:bs))

modifyBlock :: (BlockState -> BlockState) -> Codegen ()
modifyBlock f = modify (\(CodegenState i is c (b:bs)) ->
                          CodegenState i is c ((f b):bs))

addBid :: Codegen ()
addBid = modify (\(CodegenState i is c bs) ->
                            CodegenState (i + 1) is c bs)

getBid :: Codegen Int
getBid = do
  is <- gets lstack
  return $ head is

pushBid :: Codegen ()
pushBid = modify (\(CodegenState i is c bs) ->
                     CodegenState (i + 1) (i:is) c bs)

popBid :: Codegen Int
popBid = do
  i <- getBid
  modify (\(CodegenState i' (i:is) c bs) ->
             CodegenState i' is c bs)
  return i

addIns :: Instruction -> Codegen Name
addIns ins = do
  c' <- gets count
  let ref = UnName c'
  let ni = ref := ins
  modify (\(CodegenState i is' c ((BlockState x l is t):bs))
           -> CodegenState i is' (c + 1) ((BlockState x l (is ++ [ni]) t):bs))
  return ref

addStm :: Instruction -> Codegen ()
addStm ins = do
   modify (\(CodegenState i is' c ((BlockState x l is t):bs))
           -> CodegenState i is' c ((BlockState x l (is ++ [Do ins]) t):bs))

addTerm :: Terminator -> Codegen ()
addTerm t = modifyBlock (\(BlockState x l is _) ->
                           BlockState x l is (Just $ Do t))

current :: Codegen BlockState
current = head <$> (gets blocks)

--------------------------------------------------------
-- OPERANDS
--------------------------------------------------------

memPtr :: Operand
memPtr = ConstantOperand $ C.GlobalReference (ptrTy i32) $ Name "MEM_POS"

mem :: Operand
mem = ConstantOperand $ C.GlobalReference (ptrTy memTy) $ Name "MEM"

c1i8 :: Operand
c1i8 = ConstantOperand $ C.Int 8 1

c1i32 :: Operand
c1i32 = ConstantOperand $ C.Int 32 1

c0i8 :: Operand
c0i8 = ConstantOperand $ C.Int 8 0

c0i32 :: Operand
c0i32 = ConstantOperand $ C.Int 32 0

fnPutchar :: Operand
fnPutchar = ConstantOperand $ C.GlobalReference (ptrTy $ FunctionType i32 [i8] False) $ Name "putchar"

fnFflush :: Operand
fnFflush = ConstantOperand $ C.GlobalReference (ptrTy $ FunctionType i32 [ptrTy $ NamedTypeReference $ Name "FILE"] False) $ Name "fflush"

fnGetchar :: Operand
fnGetchar = ConstantOperand $ C.GlobalReference (ptrTy $ FunctionType i32 [] False) $ Name "getchar"

--------------------------------------------------------
-- INSTRUCTIONS
--------------------------------------------------------

lref :: Operand -> Name -> Operand
lref o n = LocalReference (T.typeOf o) n

ret :: Codegen ()
ret = addTerm $ Ret (Just c0i32) []

cbr :: Operand -> Name -> Name -> Codegen ()
cbr cond tl fl = addTerm $ CondBr cond tl fl []

load :: Operand -> Codegen Operand
load ptr = (LocalReference t) <$> (addIns $ Load False ptr Nothing 0 [])
  where (PointerType t _) = T.typeOf ptr

store :: Operand -> Operand -> Codegen ()
store val add = addStm $ Store False add val Nothing 0 []

add :: Operand -> Operand -> Codegen Operand
add v0 v1 = (lref v0) <$> (addIns $ Add False False v0 v1 [])

sub :: Operand -> Operand -> Codegen Operand
sub v0 v1 = (lref v0) <$> (addIns $ Sub False False v0 v1 [])

getElementPtr :: Operand -> Operand -> Codegen Operand
getElementPtr add ind = (lref add) <$> (addIns $ GetElementPtr False add [ConstantOperand $ C.Int 32 0, ind] [])

trunc :: Operand -> Type -> Codegen Operand
trunc val ty = (LocalReference ty) <$> (addIns $ Trunc val ty [])

icmp :: Operand -> Operand -> Codegen Operand
icmp v0 v1 = (LocalReference i1) <$> (addIns $ ICmp (IP.EQ) v0 v1 [])

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args = (lref fn) <$> (addIns $ Call Nothing CC.C [] (Right fn) (toArgs args) [] [])

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
