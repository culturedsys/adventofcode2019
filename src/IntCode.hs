module IntCode (
    execute,
    executeWithIo,
    loadMemory,
    dumpMemory,
    Error
) where

import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Loops (iterateWhile)
import Data.Sort (sortOn)


type Addr = Int

type Memory = M.Map Addr Int

data Error = OutOfBounds Addr Int | BadOpCode Addr Int deriving (Eq)

instance Show Error where
    show (OutOfBounds pc value) = "Out of bounds: " ++ (show value) ++ " at " ++ (show pc)
    show (BadOpCode pc value) = "Bad opcode: " ++ (show value) ++ " at " ++ (show pc)

data MachineState = MS {
    getMemory :: Memory,
    getPc :: Addr,
    getInput :: [Int],
    getOutput :: [Int]
}


type Machine a = StateT MachineState (Except Error) a

data Status = Continue | Halt

data OpCode = Add | Mul | Hlt | Inp | Out


isContinue :: Status -> Bool
isContinue Continue = True
isContinue _ = False


load :: Addr -> Machine Int
load addr = do
    memory <- getMemory <$> get
    pc <- getPc <$> get
    case M.lookup addr memory of
        Nothing -> throwError $ OutOfBounds pc addr
        Just value -> return value


store :: Addr -> Int -> Machine ()
store addr value = 
    modify (\ ms -> ms { getMemory = M.insert addr value (getMemory ms)})


operand :: Addr -> Machine Int
operand addr = load =<< load addr


performOperation :: (Int -> Int -> Int) -> Machine ()
performOperation operation = do
    pc <- getPc <$> get
    left <- operand (pc + 1)
    right <- operand (pc + 2)
    destination <- load (pc + 3)

    store destination (operation left right)


performInput :: Machine ()
performInput = do
    pc <- getPc <$> get
    dest <- load (pc + 1)
    input <- getInput <$> get

    store dest (head input)
    modify (\ms -> ms { getInput = tail input})


performOutput :: Machine ()
performOutput = do
    pc <- getPc <$> get
    source <- operand (pc + 1)
    
    modify (\ms -> ms { getOutput = source : (getOutput ms)})

    
advanceOperation :: Int -> Machine ()
advanceOperation size = modify (\ ms -> ms { getPc = (getPc ms) + size })


decode :: Int -> Maybe (OpCode, Int)
decode 1 = Just (Add, 4)
decode 2 = Just (Mul, 4)
decode 3 = Just (Inp, 2)
decode 4 = Just (Out, 2)
decode 99 = Just (Hlt, 1)
decode _ = Nothing

executeOperation :: Machine Status
executeOperation = do
    pc <- getPc <$> get
    opcode <- load pc

    (op, size) <- maybe (throwError $ BadOpCode pc opcode) return (decode opcode) 

    result <- case op of
            Add -> performOperation (+) >> return Continue
            Mul -> performOperation (*) >> return Continue
            Inp -> performInput >> return Continue
            Out -> performOutput >> return Continue
            Hlt -> return Halt
    
    advanceOperation size
    return result


dumpMemory :: Memory -> [Int]
dumpMemory memory = map snd . sortOn fst $ M.toList memory 


loadMemory :: [Int] -> Memory
loadMemory source = M.fromList $ zip [0..] source 
    

execute' :: Machine ()
execute' = do 
    iterateWhile isContinue executeOperation
    return ()


runMachine :: [Int] -> [Int] -> Either Error MachineState
runMachine program input = runExcept $ execStateT execute' (MS (loadMemory program) 0 input [])


execute :: [Int] -> Either Error [Int]
execute program = dumpMemory <$> getMemory <$> runMachine program []


executeWithIo :: [Int] -> [Int] -> Either Error [Int]
executeWithIo program input = reverse <$> getOutput <$> runMachine program input